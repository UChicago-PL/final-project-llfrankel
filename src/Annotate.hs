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

-- | Run an annotation computation with the given environment.
runAnnotation :: AnnotateEnv -> Annotator a -> a
runAnnotation env m = evalState (runReaderT m env) (AnnotateState Map.empty Set.empty)

-- | Cache the result of an annotation computation. If already computed, return
-- the cached result. If currently being computed (cycle), assume stateful.
memoized :: String -> Annotator Annotation -> Annotator Annotation
memoized key compute = do
  st <- get
  case Map.lookup key (stMemo st) of
    Just ann -> pure ann
    Nothing
      | Set.member key (stComputing st) -> pure $ mempty {aStateful = True}
      | otherwise -> do
          modify $ \s -> s {stComputing = Set.insert key (stComputing s)}
          ann <- compute
          modify $ \s ->
            s {stMemo = Map.insert key ann (stMemo s), stComputing = Set.delete key (stComputing s)}
          pure ann

-- | Build a memoization key from a bundle name and index expression.
makeKey :: String -> Expr -> String
makeKey bundle (Param field) = bundle ++ "." ++ field
makeKey bundle (Num n) = bundle ++ "." ++ show (floor n :: Int)
makeKey bundle _ = bundle

-- | Find a strand in a list by name (Param) or numeric index (Num).
findStrand :: Expr -> [Strand] -> Maybe Strand
findStrand (Param field) strands = find (\s -> strandName s == field) strands
findStrand (Num n) strands = find (\s -> strandIndex s == floor n) strands
findStrand _ _ = Nothing

-- | Look up a coordinate name and return its dimension as an annotation.
lookupCoord :: String -> AnnotateEnv -> Annotation
lookupCoord name env = case Map.lookup name (envCoords env) of
  Just dim -> mempty {aDomain = Map.singleton (dimName dim) dim}
  Nothing -> mempty

-- | Find a specific strand within a named bundle.
findBundleStrand :: String -> Expr -> AnnotateEnv -> Maybe Strand
findBundleStrand b e env = do
  bun <- Map.lookup b (envBundles env)
  findStrand e (bundleStrands bun)

-- | Compute the annotation for an expression by collecting domain dimensions,
-- hardware requirements, and statefulness from its subexpressions.
annotateExpr :: Expr -> Annotator Annotation
annotateExpr expr = case expr of
  Num _ -> pure mempty
  Param name -> asks (lookupCoord name)
  CacheRead _ _ -> pure $ mempty {aStateful = True}
  Unary _ e -> annotateExpr e
  Extract e _ -> annotateExpr e
  Binary _ l r -> liftA2 (<>) (annotateExpr l) (annotateExpr r)
  Call _ args -> mconcat <$> mapM annotateExpr args
  Builtin name args -> annotateBuiltin name args
  Index "me" (Param field) -> asks (lookupCoord field)
  Index "me" _ -> pure mempty
  Index b e -> do
    let key = makeKey b e
    memoized key $ do
      strand <- asks (findBundleStrand b e)
      maybe (pure mempty) (annotateExpr . strandExpr) strand
  Remap base subs -> annotateRemap base subs

-- | Annotate a builtin call: merge argument annotations, then override with
-- the primitive's output domain and hardware spec if one exists.
annotateBuiltin :: String -> [Expr] -> Annotator Annotation
annotateBuiltin name args = do
  merged <- mconcat <$> mapM annotateExpr args
  spec <- asks (Map.lookup name . envPrimitives)
  pure $ case spec of
    Nothing -> merged
    Just s -> overrideDomain s merged <> specAnnotation s
  where
    overrideDomain s ann = case primOutputDomain s of
      [] -> ann
      ds -> ann {aDomain = Map.fromList [(dimName d, d) | d <- ds]}
    specAnnotation s =
      mempty {aHardware = Set.fromList (primHardware s), aStateful = primAddsState s}

-- | Annotate a remap: combine base and substitution annotations, removing
-- the remapped dimensions from the base.
annotateRemap :: Expr -> Map String Expr -> Annotator Annotation
annotateRemap base subs = do
  baseAnn <- annotateExpr base
  subAnns <- mconcat <$> mapM annotateExpr (Map.elems subs)
  let removedDims = map stripMe (Map.keys subs)
  pure $ removeDims removedDims baseAnn <> subAnns
  where
    stripMe k = fromMaybe k (stripPrefix "me." k)
    removeDims dims ann = ann {aDomain = foldr Map.delete (aDomain ann) dims}

-- | Annotate every strand in the program and return the full memo table.
annotateProgram :: AnnotateEnv -> Map String Annotation
annotateProgram env = runAnnotation env $ do
  let keys =
        [ (bName ++ "." ++ strandName s, strandExpr s)
          | (bName, b) <- Map.toList (envBundles env),
            s <- bundleStrands b
        ]
  mapM_ (\(key, expr) -> memoized key (annotateExpr expr)) keys
  gets stMemo
