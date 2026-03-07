module Partition
  ( SwatchGraph (..),
    Swatch (..),
    PartInput (..),
    PartError (..),
    partition,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import DepGraph (DepGraph, transDeps)

data Swatch = Swatch
  { swatchBackend :: BackendId,
    swatchBundles :: Set BundleName,
    swatchInputs :: Set BundleName,
    swatchOutputs :: Set BundleName,
    swatchIsSink :: Bool
  }
  deriving (Show, Eq)

data SwatchGraph = SwatchGraph
  { swatches :: [Swatch],
    bundleToSwatch :: Map BundleName BackendId
  }
  deriving (Show, Eq)

data PartError
  = CycleDetected
  | HardwareConflict BundleName (Set BackendId)
  deriving (Show, Eq)

type BackendId = String

type BundleName = String

type BackendBundles = Map BackendId (Set BundleName)

type HardwareMap = Map String BackendId

type SinkMap = Map BundleName BackendId

data PartInput = PartInput
  { partGraph :: DepGraph,
    partHardware :: HardwareMap,
    partSinks :: SinkMap,
    partBuiltins :: Map BundleName (Set String)
  }

-- | Partition a program's bundles into backend-specific swatches.
partition :: PartInput -> Either PartError SwatchGraph
partition input = do
  let graph = partGraph input
      sinks = partSinks input
      claims = computeClaims input
  routes <- validateClaims claims
  let (grouped, pures) = splitRoutes routes
      expanded = expandPure graph pures grouped
      inputs = findInputs graph expanded
      outputs = findOutputs expanded inputs
  pure $ buildSwatches sinks expanded inputs outputs

-- | Which backend (if any) claim each bundle, based on hardware requirements and output assignments
computeClaims :: PartInput -> Map BundleName (Set BackendId)
computeClaims input = Map.mapWithKey claims (partBuiltins input)
  where
    claims bundle bs = hwClaims bs <> sinkClaim bundle
    hwClaims = foldMap $ \b -> foldMap Set.singleton $ Map.lookup b (partHardware input)
    sinkClaim bundle = foldMap Set.singleton $ Map.lookup bundle (partSinks input)

-- | check that no bundle is claimed by multiple backends
validateClaims :: Map BundleName (Set BackendId) -> Either PartError (Map BundleName (Maybe BackendId))
validateClaims = Map.traverseWithKey validate
  where
    validate bundle cs = case Set.toList cs of
      [] -> Right Nothing
      [bid] -> Right (Just bid)
      _ -> Left (HardwareConflict bundle cs)

-- | split validated claims into backend-grouped bundles and unclaimed (pure) bundles
splitRoutes :: Map BundleName (Maybe BackendId) -> (BackendBundles, Set BundleName)
splitRoutes routes = (grouped, pures)
  where
    grouped =
      Map.fromListWith
        Set.union
        [(bid, Set.singleton b) | (b, Just bid) <- Map.toList routes]
    pures = Set.fromList [b | (b, Nothing) <- Map.toList routes]

-- | Expand each backends bundle set to include any pure (unclaimed) bundles it transitiviely depends on
expandPure :: DepGraph -> Set BundleName -> BackendBundles -> BackendBundles
expandPure graph pures = Map.map expand
  where
    expand bundles =
      let allDeps = foldMap (transDeps graph) (Set.toList bundles)
       in bundles <> Set.intersection pures allDeps

-- | For each backend, find bundles it reads from other backends
findInputs :: DepGraph -> BackendBundles -> Map BackendId (Set BundleName)
findInputs graph = Map.map inputsOf
  where
    inputsOf bundles = depsOf bundles `Set.difference` bundles
    depsOf bundles = foldMap directDeps (Set.toList bundles)
    directDeps b = Map.findWithDefault mempty b graph

-- | For each backend, find its bundles that other backends read from
findOutputs :: BackendBundles -> Map BackendId (Set BundleName) -> Map BackendId (Set BundleName)
findOutputs expanded inputs = Map.map outputsOf expanded
  where
    allInputs = mconcat (Map.elems inputs)
    outputsOf bundles = Set.intersection bundles allInputs

-- | Assemble the final swatch graph from expanded bundles, inputs, and outputs.
buildSwatches ::
  SinkMap ->
  BackendBundles ->
  Map BackendId (Set BundleName) ->
  Map BackendId (Set BundleName) ->
  SwatchGraph
buildSwatches sinks expanded inputs outputs =
  SwatchGraph
    { swatches = map mkSwatch (Map.keys expanded),
      bundleToSwatch =
        Map.fromList
          [(b, bid) | (bid, bs) <- Map.toList expanded, b <- Set.toList bs]
    }
  where
    mkSwatch bid =
      Swatch
        { swatchBackend = bid,
          swatchBundles = bundles,
          swatchInputs = lookupSet bid inputs,
          swatchOutputs = lookupSet bid outputs,
          swatchIsSink = any (isSinkFor bid) bundles
        }
      where
        bundles = lookupSet bid expanded
    lookupSet = Map.findWithDefault mempty
    isSinkFor bid b = Map.lookup b sinks == Just bid
