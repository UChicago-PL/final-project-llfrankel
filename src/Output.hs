{-# LANGUAGE OverloadedStrings #-}

module Output where

import Annotate (Annotation (..))
import Data.Aeson
import Data.List (isPrefixOf, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import DepGraph (DepGraph)
import IR (BackendSpec (..), Dimension, Program (..))
import Partition (Swatch (..), SwatchGraph (..))

data SignalAnnotation = SignalAnnotation -- per strand output of the strand annotation
  { sigName :: String,
    sigDomain :: [Dimension],
    sigHardware :: [String],
    sigStateful :: Bool,
    sigPure :: Bool
  }

-- per-bundle summary of annotations
-- merges annotations from each comppnent
data BundleAnnotation = BundleAnnotation
  { bunName :: String,
    bunBackends :: [String],
    bunHardware :: [String],
    bunPure :: Bool,
    bunIsSink :: Bool
  }

data AnalysisOutput = AnalysisOutput
  { outSignals :: [SignalAnnotation],
    outBundles :: [BundleAnnotation],
    outSwatches :: [Swatch],
    outDependencies :: Map String [String]
  }

instance ToJSON SignalAnnotation where
  toJSON s =
    object
      [ "name" .= sigName s,
        "domain" .= sigDomain s,
        "hardware" .= sigHardware s,
        "stateful" .= sigStateful s,
        "pure" .= sigPure s
      ]

instance ToJSON BundleAnnotation where
  toJSON b =
    object
      [ "name" .= bunName b,
        "backends" .= bunBackends b,
        "hardware" .= bunHardware b,
        "pure" .= bunPure b,
        "isSink" .= bunIsSink b
      ]

instance ToJSON AnalysisOutput where
  toJSON o =
    object
      [ "signals" .= outSignals o,
        "bundles" .= outBundles o,
        "swatches" .= outSwatches o,
        "dependencies" .= outDependencies o
      ]

instance ToJSON Swatch where
  toJSON s =
    object
      [ "backend" .= swatchBackend s,
        "bundles" .= swatchBundles s,
        "inputBuffers" .= swatchInputs s,
        "outputBuffers" .= swatchOutputs s,
        "isSink" .= swatchIsSink s
      ]

-- assembles JSON-able analysis result
-- Annotations, dependency graph, swatch partitions, and backend specs
buildOutput :: Map String Annotation -> DepGraph -> SwatchGraph -> [BackendSpec] -> Program -> AnalysisOutput
buildOutput anns graph sg backends prog =
  AnalysisOutput
    { outSignals = map mkSignal (Map.toAscList anns),
      outBundles = map mkBundle (Map.keys (progBundles prog)),
      outSwatches = swatches sg,
      outDependencies = Map.map (sort . Set.toList) graph
    }
  where
    allSinks = Set.fromList (concatMap backendSinks backends)

    mkSignal (name, ann) =
      SignalAnnotation
        { sigName = name,
          sigDomain = sort (Map.elems (aDomain ann)),
          sigHardware = sort (Set.toList (aHardware ann)),
          sigStateful = aStateful ann,
          sigPure = Set.null (aHardware ann) && not (aStateful ann)
        }
    mkBundle bName =
      let merged = mconcat [ann | (k, ann) <- Map.toAscList anns, (bName ++ ".") `isPrefixOf` k]
       in BundleAnnotation
            { bunName = bName,
              bunBackends = sort [swatchBackend s | s <- swatches sg, Set.member bName (swatchBundles s)],
              bunHardware = sort (Set.toList (aHardware merged)),
              bunPure = Set.null (aHardware merged) && not (aStateful merged),
              bunIsSink = Set.member bName allSinks
            }
