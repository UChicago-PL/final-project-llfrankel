{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Annotate
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import DepGraph
import IR
import IRHelpers
import Output
import Partition

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
          hwMap = Map.fromList [(hw, backendId b) | b <- backends, hw <- backendHardware b]
          sinkMap = Map.fromList [(s, backendId b) | b <- backends, s <- backendSinks b]
          builtins = Map.map (bundleHardware prims) (progBundles prog)
          partInput = PartInput graph hwMap sinkMap builtins
      case partition partInput of
        Left err -> putStrLn $ "PARTITION FAILED: " ++ show err
        Right sg -> BL.putStr $ encode (buildOutput annotations graph sg backends prog)
