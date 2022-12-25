module Main where

import Shuffle (shuffleM)
import Control.Monad (liftM)
import Data.Time.Clock
import Data.Vector
import Prelude hiding (head)

import GA
import TSP

generationNum = 100

inputPath :: IO (Vector Coord)
inputPath = do
--  contents <- getContents
  contents <- readFile "input/48node.txt"
  return $ fromList $ toCoord . words <$> lines contents
    where
      toCoord :: [String] -> Coord
      toCoord [s1, s2] = Coord (read s1, read s2)
      toCoord _ = error "it is illegal file format!"

main :: IO ()
main = do
  start <- getCurrentTime

  coords <- inputPath

  paths <- replicateM poolSize $ Path <$> shuffleM coords
  elite <- geneticLoop generationNum paths (head paths)

  print $ totalCost elite

  end <- getCurrentTime
  print $ diffUTCTime end start
