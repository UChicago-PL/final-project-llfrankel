{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Annotate
import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import DepGraph
import IR
import IRHelpers

-- right now this just outputs the regular JSON so I can check if its right
main :: IO ()
main = do
  raw <- BL.getContents
  case decode raw :: Maybe AnalysisInput of
    Nothing -> putStrLn "PARSE FAILED"
    Just input -> do
      let prog = inputProgram input
          backends = inputBackends input
          coords = foldMap backendCoords backends
          prims = foldMap backendPrimitives backends
          env = AnnotateEnv (progBundles prog) coords prims
          annotations = annotateProgram env
          graph = buildGraph prog
      putStrLn $ "Bundles: " ++ show (Map.keys (progBundles prog))
      putStrLn $ "Graph: " ++ show graph
      putStrLn $ "Annotations: " ++ show annotations
