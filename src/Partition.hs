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
import DepGraph (transDeps)

type BackendId = String

type BundleName = String

type BackendBundles = Map BackendId (Set BundleName)

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

data PartInput = PartInput
  { partGraph :: Map BundleName (Set BundleName),
    partHardware :: Map String BackendId,
    partSinks :: Map BundleName BackendId,
    partBuiltins :: Map BundleName (Set String)
  }

computeClaims :: PartInput -> Map BundleName (Set BackendId)
computeClaims input = Map.mapWithKey claims (partBuiltins input)
  where
    claims bundle bs = hwClaims bs <> sinkClaim bundle
    hwClaims = foldMap hwLookup
    hwLookup b = maybe mempty Set.singleton (Map.lookup b (partHardware input))
    sinkClaim bundle = maybe mempty Set.singleton (Map.lookup bundle (partSinks input))

validateClaims ::
  Map BundleName (Set BackendId) ->
  Either PartError (Map BundleName (Maybe BackendId))
validateClaims = Map.traverseWithKey validate
  where
    validate bundle cs = case Set.toList cs of
      [] -> Right Nothing
      [bid] -> Right (Just bid)
      _ -> Left (HardwareConflict bundle cs)

splitRoutes :: Map BundleName (Maybe BackendId) -> (BackendBundles, Set BundleName)
splitRoutes routes = (grouped, pures)
  where
    grouped =
      Map.fromListWith
        Set.union
        [(bid, Set.singleton b) | (b, Just bid) <- Map.toList routes]
    pures = Set.fromList [b | (b, Nothing) <- Map.toList routes]

expandPure :: PartInput -> Set BundleName -> BackendBundles -> BackendBundles
expandPure input pures = Map.map expand
  where
    expand bundles =
      let allDeps = foldMap (transDeps (partGraph input)) (Set.toList bundles)
       in bundles <> Set.intersection pures allDeps

findInputs :: PartInput -> BackendBundles -> Map BackendId (Set BundleName)
findInputs input = Map.map inputsOf
  where
    inputsOf bundles = depsOf bundles `Set.difference` bundles
    depsOf bundles = foldMap directDeps (Set.toList bundles)
    directDeps b = Map.findWithDefault mempty b (partGraph input)

findOutputs :: BackendBundles -> Map BackendId (Set BundleName) -> Map BackendId (Set BundleName)
findOutputs expanded inputs = Map.map outputsOf expanded
  where
    allInputs = mconcat (Map.elems inputs)
    outputsOf bundles = Set.intersection bundles allInputs

buildSwatches ::
  PartInput ->
  BackendBundles ->
  Map BackendId (Set BundleName) ->
  Map BackendId (Set BundleName) ->
  SwatchGraph
buildSwatches input expanded inputs outputs =
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
    isSinkFor bid b = Map.lookup b (partSinks input) == Just bid

partition :: PartInput -> Either PartError SwatchGraph
partition input = do
  let claims = computeClaims input
  routes <- validateClaims claims
  let (grouped, pures) = splitRoutes routes
      expanded = expandPure input pures grouped
      inputs = findInputs input expanded
      outputs = findOutputs expanded inputs
  Right (buildSwatches input expanded inputs outputs)
