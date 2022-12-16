module Data.Wine
  ( data'
  ) where

import           Data.List.Split

parseOutput :: String -> [Double]
parseOutput class' =
  case class' of
    "1" -> [1, 0, 0]
    "2" -> [0, 1, 0]
    "3" -> [0, 0, 1]
    _   -> [1, 1, 1]

parseLine :: String -> ([Double], [Double])
parseLine line =
  let pLine = splitOn "," line
      output = parseOutput . head $ pLine
      input = map read (tail pLine) :: [Double]
   in (input, output)

data' :: IO [([Double], [Double])]
data' = do
  readData <- readFile "res/learn/wine.data"
  return $ map parseLine . lines $ readData
