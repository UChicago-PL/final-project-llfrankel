{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (Value (..), decode)
import Data.Aeson.Key (fromText, toText)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate, sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Environment (getArgs)

field :: Text -> Value -> Value
field k (Object obj) = fromMaybe Null $ KM.lookup (fromText k) obj
field _ _ = Null

arrayField :: Text -> Value -> [Value]
arrayField k v = case field k v of
  Array arr -> V.toList arr
  _ -> []

objectField :: Text -> Value -> [(Text, Value)]
objectField k v = case field k v of
  Object obj -> [(toText k', v') | (k', v') <- KM.toList obj]
  _ -> []

fmt :: Value -> String
fmt (String t) = T.unpack t
fmt (Bool b) = show b
fmt (Number n) = show n
fmt Null = "null"
fmt (Array arr) = "[" ++ intercalate ", " (map fmt (V.toList arr)) ++ "]"
fmt (Object obj) = "{" ++ intercalate ", " [T.unpack (toText k) ++ ": " ++ fmt v | (k, v) <- KM.toList obj] ++ "}"

diffEntities :: Text -> String -> [Value] -> [Value] -> IO ()
diffEntities kField name swList hsList = do
  let toMap xs = Map.fromList [(fmt (field kField x), x) | x <- xs]
      sm = toMap swList
      hm = toMap hsList
      allKeys = sort $ Map.keys $ Map.union hm sm
  mapM_ (checkOne sm hm) allKeys -- mapM_ reccomended by HLS, pretty simple
  where
    checkOne sm hm k = case (Map.lookup k sm, Map.lookup k hm) of
      (Nothing, _) -> putStrLn $ "  \"" ++ k ++ "\": only in Haskell"
      (_, Nothing) -> putStrLn $ "  \"" ++ k ++ "\": only in Swift"
      (Just (Object s), Just (Object h)) ->
        case fieldDiffs s h of
          [] -> pure ()
          ds -> do
            putStrLn $ "  \"" ++ k ++ "\":"
            mapM_ putStrLn ds
      _ -> pure ()
    fieldDiffs s h = checkFields (Set.toList $ Set.fromList $ map toText $ KM.keys s ++ KM.keys h)
      where
        checkFields [] = []
        checkFields (f : fs)
          | f == kField = checkFields fs
          | sv /= hv = ("    " ++ T.unpack f ++ ":  Swift=" ++ fmt sv ++ "  Haskell=" ++ fmt hv) : checkFields fs
          | otherwise = checkFields fs
          where
            sv = fromMaybe Null $ KM.lookup (fromText f) s
            hv = fromMaybe Null $ KM.lookup (fromText f) h

diffDeps :: [(Text, Value)] -> [(Text, Value)] -> IO ()
diffDeps swiftDeps haskDeps = do
  let sm = Map.fromList [(T.unpack k, v) | (k, v) <- swiftDeps]
      hm = Map.fromList [(T.unpack k, v) | (k, v) <- haskDeps]
      allKeys = sort $ Map.keys $ Map.union sm hm
  mapM_ (checkOne sm hm) allKeys
  where
    checkOne sm hm k = case (Map.lookup k sm, Map.lookup k hm) of
      (Nothing, _) -> putStrLn $ "  \"" ++ k ++ "\": only in Haskell"
      (_, Nothing) -> putStrLn $ "  \"" ++ k ++ "\": only in Swift"
      (Just sv, Just hv)
        | sv /= hv -> do
            putStrLn $ "  \"" ++ k ++ "\":"
            putStrLn $ "    Swift=" ++ fmt sv ++ "  Haskell=" ++ fmt hv
      _ -> pure ()

main :: IO ()
main = do
  [swiftPath, haskPath] <- getArgs
  swiftRaw <- BL.readFile swiftPath
  haskRaw <- BL.readFile haskPath
  let swift = case decode swiftRaw of
        Just v -> v
        Nothing -> error $ "Failed to parse: " ++ swiftPath
      hask = case decode haskRaw of
        Just v -> v
        Nothing -> error $ "Failed to parse: " ++ haskPath
  diffEntities "name" "signals" (arrayField "signals" swift) (arrayField "signals" hask)
  diffEntities "name" "bundles" (arrayField "bundles" swift) (arrayField "bundles" hask)
  diffEntities "backend" "swatches" (arrayField "swatches" swift) (arrayField "swatches" hask)
  diffDeps (objectField "dependencies" swift) (objectField "dependencies" hask)
