{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as BL
import IR

-- right now this just outputs the regular JSON so I can check if its right
main :: IO ()
main = do
  raw <- BL.getContents
  case decode raw :: Maybe AnalysisInput of
    Nothing -> putStrLn "PARSE FAILED"
    Just input -> BL.putStr (encode input)
