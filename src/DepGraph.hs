module DepGraph
  ( DepGraph,
    buildGraph,
    topoSort,
    dependents,
    transDeps,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import IR
import IRHelpers

type DepGraph = Map String (Set String)

-- | Build a dependency graph from a program. Each bundle maps to the
-- set of other bundles it references. Self-references are excluded.
buildGraph :: Program -> DepGraph
buildGraph prog = Map.mapWithKey Set.delete raw
  where
    raw = Map.map refsForBundle $ progBundles prog
    refsForBundle b = foldMap (bundleRefs . strandExpr) (bundleStrands b)

topoSort :: DepGraph -> Maybe [String]
topoSort graph = reverse <$> visit (Map.keys graph) Set.empty Set.empty []
  where
    visit [] _ _ result = Just result
    visit (n : ns) visited visiting result
      | Set.member n visited = visit ns visited visiting result
      | Set.member n visiting = Nothing
      | otherwise = do
          let deps = Map.findWithDefault Set.empty n graph
              visiting' = Set.insert n visiting
          result' <- visit (Set.toList deps) visited visiting' result
          visit ns (Set.insert n visited) (Set.delete n visiting') (n : result')

-- | Reverse the dependency graph: for each bundle, which bundles depend on it.
dependents :: DepGraph -> DepGraph
dependents graph =
  Map.fromListWith
    Set.union
    [ (dep, Set.singleton name)
      | (name, deps) <- Map.toList graph,
        dep <- Set.toList deps
    ]

-- | All bundles reachable from a starting node, transitively.
transDeps :: DepGraph -> String -> Set String
transDeps graph start = dfs Set.empty (Map.findWithDefault Set.empty start graph)
  where
    dfs = Set.foldl' visit
    visit seen n
      | Set.member n seen = seen
      | otherwise = dfs (Set.insert n seen) (Map.findWithDefault Set.empty n graph)
