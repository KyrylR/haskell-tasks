module Data.Car
  ( data'
  ) where

import           Data.List.Split
import Data.List

parseOutput :: String -> [Double]
parseOutput class' =
  case class' of
    "unacc" -> [1, 0, 0, 0]
    "acc"   -> [0, 1, 0, 0]
    "good"  -> [0, 0, 1, 0]
    "vgood" -> [0, 0, 0, 1]

--    _ -> []
parseBuyingOrMaint :: String -> Double
parseBuyingOrMaint attr =
  case attr of
    "vhigh" -> 4
    "high"   -> 3
    "med"    -> 2
    "low"    -> 1
--    _ -> -1

parseDoors :: String -> Double
parseDoors attr =
  case attr of
    "2"     -> 4
    "3"     -> 3
    "4"     -> 2
    "5more" -> 1
--    _ -> -1

parsePersons :: String -> Double
parsePersons attr =
  case attr of
    "2"    -> 1
    "4"    -> 2
    "more" -> 3
--    _ -> -1

parseLugBoot :: String -> Double
parseLugBoot attr =
  case attr of
    "small" -> 1
    "med"   -> 2
    "big"   -> 3
--    _ -> -1

parseSafety :: String -> Double
parseSafety attr =
  case attr of
    "low"  -> 1
    "med"  -> 2
    "high" -> 3
--    _ -> -1

parseLine :: String -> ([Double], [Double])
parseLine line =
  let pLine = splitOn "," line
      output = parseOutput . last $ pLine
      input =
        (\[buying, maint, doors, persons, lug_boot, safety] ->
           [ parseBuyingOrMaint buying
           , parseBuyingOrMaint maint
           , parseDoors doors
           , parsePersons persons
           , parseLugBoot lug_boot
           , parseSafety safety
           ])
          (init pLine)
   in (input, output)

data' :: IO [([Double], [Double])]
data' = do
  readData <- readFile "res/learn/car.data"
  return $ sortOn snd . map parseLine . lines $ readData
