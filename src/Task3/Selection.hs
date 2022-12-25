module Selection (
  randomSelect,
  rouletteSelectPapaMama,
  rouletteSelect,
  rankingSelect,
  tournamentSelect
)where

import System.Random
import Control.Monad.State hiding (replicateM)
import Data.Vector
import Data.List (sort)

import Prelude hiding (
  length,
  sum,
  map,
  scanl1,
  last,
  zip,
  take,
  minimum
  )

import TSP

tournamentSelect :: Int -> Int -> Vector Path -> IO (Vector Path)
tournamentSelect poolSize n paths = replicateM poolSize $ do
  randomSelectPath <- replicateM n (randomSelectOne paths)
  return $ minimum randomSelectPath

randomSelectOne :: Vector Path -> IO Path
randomSelectOne paths = do
  gen <- newStdGen
  let n = length paths
      (index, _) = randomR (0, n - 1) gen :: (Int, StdGen)
  return $ paths ! index

rankingSelect :: Int -> Vector Path -> IO (Vector Path)
rankingSelect poolSize paths =
  return $ take poolSize $ fromList $ sort $ toList paths

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt (a,b) = state $ randomR (a,b)

twoRandomNumState :: (Random a) => (a,a) -> State StdGen (a, a)
twoRandomNumState fromTo = do
  n1 <- randomRSt fromTo
  n2 <- randomRSt fromTo
  return (n1, n2)

twoRandomNum :: (Random a) => (a,a) -> StdGen -> (a,a)
twoRandomNum fromTo = evalState (twoRandomNumState fromTo)

randomSelect :: Vector Path -> IO (Path, Path)
randomSelect paths = do
  gen <- newStdGen
  let n = length paths
      (r1, r2) = twoRandomNum (0, n - 1) gen
  return (paths ! r1, paths ! r2)

-- CAUTION
-- if roulette value of one solution is very large, this function can enter into infinite loop !!
rouletteSelectPapaMama :: Vector Path -> IO (Path, Path)
rouletteSelectPapaMama chromosomes = do
  papa <- rouletteSelect chromosomes
  mama <- rouletteSelect chromosomes
  if papa == mama
    then rouletteSelectPapaMama chromosomes
    else return (papa, mama)

rouletteSelect :: Vector Path -> IO Path
rouletteSelect chromosomes = do
  gen <- newStdGen
  let fitness =  map totalCost chromosomes :: Vector Double
      total = sum fitness --累積値
      reverseFit = map (total /) fitness :: Vector Double
      roulette = scanl1 (+) reverseFit
      (ball, _) = randomR (0, last roulette) gen :: (Double, StdGen)
      Just (path, _) = find (\(_, r) -> r > ball) $ zip chromosomes roulette :: Maybe (Path, Double)
  return path
