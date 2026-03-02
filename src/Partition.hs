module Partition
  ( SwatchGraph (..),
    Swatch (..),
    partition,
  )
where

import Annotate (Annotation (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import DepGraph (transDeps)

type BackendId = String

type BundleName = String

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
