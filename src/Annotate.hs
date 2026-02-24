module Annotate where

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import IR

data Annotation = Annotation
  { aDomain :: [Dimension],
    aHardware :: Set String,
    aStateful :: Bool
  }
  deriving (Show, Eq)

emptyA :: Annotation
emptyA = Annotation [] Set.empty False

merge :: Annotation -> Annotation -> Annotation
merge a b =
  Annotation
    { aDomain = mergeDomains (aDomain a) (aDomain b),
      aHardware = Set.union (aHardware a) (aHardware b),
      aStateful = aStateful a || aStateful b
    }

mergeDomains :: [Dimension] -> [Dimension] -> [Dimension]
mergeDomains as bs = Map.elems merged
  where
    toMap ds = Map.fromList [(dimName d, d) | d <- ds]
    merged = Map.unionWith pickRestrict (toMap as) (toMap bs)
    pickRestrict a b
      | dimAccess a == Bound || dimAccess b == Bound = a {dimAccess = Bound}
      | otherwise = a
