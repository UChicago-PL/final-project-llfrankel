module Main where

import AnnotateTest (annotateTests)
import DepGraphTest (depGraphTests)
import PartitionTest (partitionTests)

main :: IO ()
main = do
  depGraphTests
  partitionTests
  annotateTests
