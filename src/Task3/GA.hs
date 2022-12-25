module GA (
  poolSize,
  geneticLoop
)where

import Data.Vector
import Text.Printf
import System.Random
import Prelude
       hiding (minimum, length, foldl, head, tail, concatMap)
import TSP
import Shuffle

import CrossOver (partiallyFitCrossOver,
                  partiallyFitConstCrossOver,
                  orderCrossOver,
                  )
import Selection

poolSize = 1000 :: Int
matingSelection = randomSelect
--crossOver = partiallyFitConstCrossOver 3
--crossOver = partiallyFitCrossOver
crossOver = orderCrossOver
mutationRate = 0.1
childrenSelect = tournamentSelect poolSize 3

mating :: Vector Path -> IO (Vector Path)
mating parents = do
  childList2 <- replicateM (length parents) $ do
    (papa, mama) <- matingSelection parents
    crossOver papa mama :: IO (Vector Path)
  return $ concatVector childList2

concatVector :: Vector (Vector a) -> Vector a
concatVector = concatMap id

mutation :: Vector Path -> IO (Vector Path)
mutation children = do
  gen <- newStdGen
  return $ fst $ foldl mutate (empty, gen) children
  where
    mutate :: (Vector Path, StdGen) -> Path -> (Vector Path, StdGen)
    mutate (ps, gen) p = do
      let (r, newG) = randomR (0, 1) gen :: (Double, StdGen)
      if r < mutationRate
        then (shufflePath p newG `cons` ps, newG)
        else (p `cons` ps, newG)

selectChildren :: Vector Path -> IO (Vector Path)
selectChildren = childrenSelect

geneticLoop :: Int -> Vector Path -> Path -> IO Path
geneticLoop 0 _ elite = return elite
geneticLoop generation parents elite = do
  newParents <- mating parents >>=
                mutation >>=
                selectChildren

  let minPath = minimum newParents :: Path
      newElite = min minPath elite :: Path

  printf "%d generation , %f , %f\n" generation (totalCost newElite) (totalCost minPath)

  geneticLoop (generation - 1) newParents newElite