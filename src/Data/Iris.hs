module Data.Iris
  ( data'
  ) where

import           Data.List.Split

parseOutput :: String -> [Double]
parseOutput class' =
  case class' of
    "Iris-setosa"     -> [1, 0, 0]
    "Iris-versicolor" -> [0, 1, 0]
    "Iris-virginica"  -> [0, 0, 1]
    _                 -> [1, 1, 1]

parseLine :: String -> ([Double], [Double])
parseLine line =
  let pLine = splitOn "," line
      output = parseOutput . last $ pLine
      input = map read (init pLine) :: [Double]
   in (input, output)

data' :: IO [([Double], [Double])]
data' = do
  readData <- readFile "res/learn/iris.data"
  return $ map parseLine . lines $ readData
