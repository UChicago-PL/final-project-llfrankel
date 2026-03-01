module Annotate
  ( Annotation (..),
    AnnotateEnv (..),
    annotateProgram,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Data.List (find, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import IR

data Annotation = Annotation
  { aDomain :: Map String Dimension,
    aHardware :: Set String,
    aStateful :: Bool
  }
  deriving (Show, Eq)

instance Semigroup Annotation where
  a <> b =
    Annotation
      { aDomain = Map.unionWith pick (aDomain a) (aDomain b),
        aHardware = aHardware a <> aHardware b,
        aStateful = aStateful a || aStateful b
      }
    where
      pick d1 d2
        | dimAccess d1 == Bound || dimAccess d2 == Bound = d1 {dimAccess = Bound}
        | otherwise = d1

instance Monoid Annotation where
  mempty = Annotation Map.empty Set.empty False

data AnnotateEnv = AnnotateEnv
  { envBundles :: Map String Bundle,
    envCoords :: Map String Dimension,
    envPrimitives :: Map String PrimitiveSpec
  }

data AnnotateState = AnnotateState
  { stMemo :: Map String Annotation,
    stComputing :: Set String
  }

type Annotator a = ReaderT AnnotateEnv (State AnnotateState) a

runAnnotation :: AnnotateEnv -> Annotator a -> a
runAnnotation env m = evalState (runReaderT m env) (AnnotateState Map.empty Set.empty)

memoized :: String -> Annotator Annotation -> Annotator Annotation
memoized key compute = do
  st <- get
  case Map.lookup key (stMemo st) of
    Just ann -> return ann
    Nothing
      | Set.member key (stComputing st) -> return $ mempty {aStateful = True}
      | otherwise -> do
          modify $ \s -> s {stComputing = Set.insert key (stComputing s)}
          ann <- compute
          modify $ \s ->
            s {stMemo = Map.insert key ann (stMemo s), stComputing = Set.delete key (stComputing s)}
          return ann

makeKey :: String -> Expr -> String
makeKey bundle (Param field) = bundle ++ "." ++ field
makeKey bundle (Num n) = bundle ++ "." ++ show (floor n :: Int)
makeKey bundle _ = bundle

findStrand :: Expr -> [Strand] -> Maybe Strand
findStrand (Param field) strands = find (\s -> strandName s == field) strands
findStrand (Num n) strands = find (\s -> strandIndex s == floor n) strands
findStrand _ _ = Nothing

lookupCoord :: String -> AnnotateEnv -> Annotation
lookupCoord name env = case Map.lookup name (envCoords env) of
  Just dim -> mempty {aDomain = Map.singleton (dimName dim) dim}
  Nothing -> mempty

findBundleStrand :: String -> Expr -> AnnotateEnv -> Maybe Strand
findBundleStrand b e env = do
  bun <- Map.lookup b (envBundles env)
  findStrand e (bundleStrands bun)

annotateExpr :: Expr -> Annotator Annotation
annotateExpr expr = case expr of
  Num _ -> return mempty
  Param name -> asks (lookupCoord name)
  CacheRead _ _ -> return $ mempty {aStateful = True}
  Unary _ e -> annotateExpr e
  Extract e _ -> annotateExpr e
  Binary _ l r -> (<>) <$> annotateExpr l <*> annotateExpr r
  Call _ args -> mconcat <$> mapM annotateExpr args
  Builtin name args -> annotateBuiltin name args
  Index "me" (Param field) -> asks (lookupCoord field)
  Index "me" _ -> return mempty
  Index b e -> do
    let key = makeKey b e
    memoized key $ do
      strand <- asks (findBundleStrand b e)
      maybe (return mempty) (annotateExpr . strandExpr) strand
  Remap base subs -> annotateRemap base subs

annotateBuiltin :: String -> [Expr] -> Annotator Annotation
annotateBuiltin name args = do
  merged <- mconcat <$> mapM annotateExpr args
  spec <- asks (Map.lookup name . envPrimitives)
  return $ case spec of
    Nothing -> merged
    Just s ->
      Annotation
        { aDomain = case primOutputDomain s of
            [] -> aDomain merged
            ds -> Map.fromList [(dimName d, d) | d <- ds],
          aHardware = aHardware merged <> Set.fromList (primHardware s),
          aStateful = aStateful merged || primAddsState s
        }

annotateRemap :: Expr -> Map String Expr -> Annotator Annotation
annotateRemap base subs = do
  baseAnn <- annotateExpr base
  subAnns <- mconcat <$> mapM annotateExpr (Map.elems subs)
  let removedDims = map (fromMaybe <*> stripPrefix "me.") (Map.keys subs)
      baseDomain = foldr Map.delete (aDomain baseAnn) removedDims
  return $
    Annotation
      { aDomain = baseDomain <> aDomain subAnns,
        aHardware = aHardware baseAnn <> aHardware subAnns,
        aStateful = aStateful baseAnn || aStateful subAnns
      }

annotateProgram :: AnnotateEnv -> Map String Annotation
annotateProgram env = runAnnotation env $ do
  let keys =
        [ (bName ++ "." ++ strandName s, strandExpr s)
          | (bName, b) <- Map.toList (envBundles env),
            s <- bundleStrands b
        ]
  mapM_ (\(key, expr) -> memoized key (annotateExpr expr)) keys
  gets stMemo
