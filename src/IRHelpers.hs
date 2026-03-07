-- HLS reccomended this guy, seems useful
{-# LANGUAGE LambdaCase #-}

module IRHelpers (bundleRefs, exprBuiltins, bundleBuiltins, bundleHardware) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import IR

bundleBuiltins :: Bundle -> Set String
bundleBuiltins b = foldMap (exprBuiltins . strandExpr) (bundleStrands b)

bundleHardware :: Map String PrimitiveSpec -> Bundle -> Set String
bundleHardware prims b =
  foldMap (\name -> maybe Set.empty (Set.fromList . primHardware) (Map.lookup name prims))
    (bundleBuiltins b)

foldMapExpr :: (Monoid m) => (Expr -> m) -> Expr -> m
foldMapExpr f expr =
  f expr <> case expr of
    Binary _ l r -> foldMapExpr f l <> foldMapExpr f r
    Unary _ e -> foldMapExpr f e
    Call _ args -> foldMap (foldMapExpr f) args
    Builtin _ args -> foldMap (foldMapExpr f) args
    Index _ e -> foldMapExpr f e
    Extract e _ -> foldMapExpr f e
    Remap b subs -> foldMapExpr f b <> foldMap (foldMapExpr f) (Map.elems subs)
    _ -> mempty

exprBuiltins :: Expr -> Set String
exprBuiltins = foldMapExpr $ \case
  Builtin name _ -> Set.singleton name
  _ -> mempty

bundleRefs :: Expr -> Set String
bundleRefs = foldMapExpr $ \case
  Index b _ | b /= "me" -> Set.singleton b
  _ -> mempty
