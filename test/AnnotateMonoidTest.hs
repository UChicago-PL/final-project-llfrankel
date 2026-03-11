-- | testing if Annotations behave like Semigroups/monoids, as they should
module AnnotateMonoidTest (annotateMonoidTests) where

import Annotate
import qualified Data.Map as Map
import qualified Data.Set as Set
import IR (Access (..), Dimension (..))
import Test.QuickCheck

instance Arbitrary Access where
  arbitrary = elements [Free, Bound]

instance Arbitrary Dimension where
  arbitrary = Dimension <$> elements ["x", "y", "z", "t", "k"] <*> arbitrary

instance Arbitrary Annotation where
  arbitrary = Annotation <$> arbDomain <*> arbHardware <*> arbitrary
    where
      arbDomain = Map.fromList <$> listOf arbDimEntry
      arbDimEntry = (,) <$> arbName <*> arbitrary
      arbHardware = Set.fromList <$> sublistOf ["gpu", "cpu", "display", "mic"]
      arbName = elements ["x", "y", "z", "t", "k"]

propBoundDominates :: Annotation -> Annotation -> Bool
propBoundDominates a b = all checkDim allDims
  where
    merged = a <> b
    allDims = Map.keys (aDomain a) ++ Map.keys (aDomain b)
    isBound name ann = maybe False ((== Bound) . dimAccess) $ Map.lookup name (aDomain ann)
    checkDim name
      | isBound name a || isBound name b = isBound name merged
      | otherwise = True

propDomainPreserved :: Annotation -> Annotation -> Bool
propDomainPreserved a b =
  let merged = a <> b
      allKeys = Set.union (Map.keysSet (aDomain a)) $ Map.keysSet (aDomain b)
   in allKeys == Map.keysSet (aDomain merged)

propStatefulPropagates :: Annotation -> Annotation -> Bool
propStatefulPropagates a b = not (aStateful a || aStateful b) || aStateful (a <> b)

propHardwareUnion :: Annotation -> Annotation -> Bool
propHardwareUnion a b = aHardware (a <> b) == Set.union (aHardware a) (aHardware b)

propAssoc :: Annotation -> Annotation -> Annotation -> Bool
propAssoc a b c = (a <> b) <> c == a <> (b <> c)

propIdentLeft :: Annotation -> Bool
propIdentLeft a = mempty <> a == a

propIdentRight :: Annotation -> Bool
propIdentRight a = a <> mempty == a

annotateMonoidTests :: IO ()
annotateMonoidTests = do
  putStrLn "\n ====== Annotation Monoid ======"
  quickCheck propBoundDominates
  quickCheck propDomainPreserved
  quickCheck propStatefulPropagates
  quickCheck propHardwareUnion
  quickCheck propAssoc
  quickCheck propIdentLeft
  quickCheck propIdentRight
  putStrLn "\n\n"
