{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as BL
import IR

main :: IO ()
main = do
  raw <- BL.getContents
  case decode raw :: Maybe AnalysisInput of
    Nothing -> putStrLn "PARSE FAILED"
    Just input -> BL.putStr (encode input)
