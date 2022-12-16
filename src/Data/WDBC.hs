module Data.WDBC
  ( data'
  ) where

import           Data.List
import           Data.List.Split

parseOutput :: String -> [Double]
parseOutput class' =
  case class' of
    "M" -> [1, 0]
    "B" -> [0, 1]

parseLine :: String -> ([Double], [Double])
parseLine line =
  let pLine = tail $ splitOn "," line
      output = parseOutput . head $ pLine
      input = map read (tail pLine) :: [Double]
   in (input, output)

data' :: IO [([Double], [Double])]
data' = do
  readData <- readFile "res/learn/wdbc.data"
  return $ sortOn snd . map parseLine . lines $ readData
