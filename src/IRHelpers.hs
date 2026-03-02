module IRHelpers (bundleRefs, exprBuiltins, bundleBuiltins) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import IR

bundleBuiltins :: Bundle -> Set String
bundleBuiltins b = foldMap (exprBuiltins . strandExpr) (bundleStrands b)

exprBuiltins :: Expr -> Set String
exprBuiltins expr = case expr of
  Builtin name args -> Set.insert name (foldMap exprBuiltins args)
  Binary _ l r -> exprBuiltins l <> exprBuiltins r
  Unary _ e -> exprBuiltins e
  Call _ args -> foldMap exprBuiltins args
  Index _ e -> exprBuiltins e
  Extract e _ -> exprBuiltins e
  Remap b subs -> exprBuiltins b <> foldMap exprBuiltins (Map.elems subs)
  _ -> mempty

bundleRefs :: Expr -> Set String
bundleRefs expr = case expr of
  Num _ -> mempty
  Param _ -> mempty
  CacheRead _ _ -> mempty
  Index "me" e -> bundleRefs e
  Index b e -> Set.insert b $ bundleRefs e
  Binary _ l r -> bundleRefs l <> bundleRefs r
  Unary _ e -> bundleRefs e
  Call _ args -> foldMap bundleRefs args
  Builtin _ args -> foldMap bundleRefs args
  Extract e _ -> bundleRefs e
  Remap b subs -> bundleRefs b <> foldMap bundleRefs (Map.elems subs)
