module CrossOver (
  partiallyFitCrossOver,
  partiallyFitConstCrossOver,
  orderCrossOver,
  nub
)where

import System.Random
import Control.Monad.State
import Data.Vector
import Text.Printf
--import Debug.Trace (trace)

import Prelude
       hiding (length, zip, map, splitAt, filter, notElem, (++), nub,
               head, tail, enumFromTo)

import TSP
import Shuffle

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt (a,b) = state $ randomR (a,b)

twoRandomDivState :: (Int, Int) -> State StdGen (Int, Int)
twoRandomDivState (from, to) = do
  n1 <- randomRSt (from, to)
  n2 <- randomRSt (n1, to)
  return (n1, n2 - n1 + 1)

twoRandomNum :: (Int, Int) -> StdGen -> (Int, Int)
twoRandomNum fromTo = evalState (twoRandomDivState fromTo)

partiallyFitCrossOver :: Path -> Path -> IO (Vector Path)
partiallyFitCrossOver papa@(Path coords) mama = do
  gen <- newStdGen
  let n = length coords
      (divPoint1, divNum) = twoRandomNum (1, n - 1) gen
  return $ exchangePartial papa mama divPoint1 divNum

--changeRange = 1
partiallyFitConstCrossOver :: Int -> Path -> Path -> IO (Vector Path)
partiallyFitConstCrossOver changeRange papa@(Path coords) mama = do
  gen <- newStdGen
  let n = length coords
      (divPoint1, newGen) = randomR (1, n - 1) gen :: (Int, StdGen)
      divNum = if divPoint1 + changeRange > n then n - divPoint1 else changeRange
  return $ exchangePartial papa mama divPoint1 divNum

exchangePartial :: Path -> Path -> Int -> Int -> Vector Path
exchangePartial (Path papaCoords) (Path mamaCoords) point1 numRange =
  let div1 = slice point1 numRange papaCoords :: Vector Coord
      div2 = slice point1 numRange mamaCoords :: Vector Coord
      changePair = nub $ map (\(a,b) -> (max a b, min a b)) $ zip div1 div2 :: Vector (Coord, Coord) -- タプルの内部をソートして、重複を削除
  in map (Path . exchange papaCoords) changePair

-- | 重複削除
--
-- >>> nub $ fromList [0,1,2,0,3,3,2,4,2,4,5]
-- [0,1,2,3,4,5]
-- >>> nub $ fromList [0,0,0,0,0,0,0,0,0,0]
-- [0]
-- >>> nub empty
-- []
nub :: (Eq a) => Vector a -> Vector a
nub list
  | list == empty = empty
  | otherwise  = x `cons` nub (filter (/=x) xs)
  where
    x = head list
    xs = tail list

-- | リストの要素同士を入れ替える
--
-- >>> exchange (fromList [1..5]) (2,5)
-- [1,5,3,4,2]
--
-- >>> exchange empty (2,5)
-- []
exchange :: (Eq a) => Vector a -> (a, a) -> Vector a
exchange list (a,b)
  | list == empty = empty
  | otherwise =
      let (Just fstIndex) = elemIndex a list
          (Just sndIndex) = elemIndex b list
      in swap fstIndex sndIndex list

-- | インデックスアクセスでリストの要素を入れ替える
--
-- >>> swap 1 3 (enumFromTo 0 5)
-- [0,3,2,1,4,5]
--
-- >>> swap 0 0 (enumFromTo 0 5)
-- [0,1,2,3,4,5]
swap :: Int -> Int -> Vector a -> Vector a
swap first second list = list // [(first, list ! second), (second, list ! first)]

orderCrossOver :: Path -> Path -> IO (Vector Path)
orderCrossOver papa@(Path coords) mama = do
  gen <- newStdGen
  let n = length coords
      (dividePoint,_) = randomR (1, n - 1) gen :: (Int, StdGen)
  return $ divCpChromo dividePoint mama papa
--  return $ shuffleCp dividePoint mama papa gen

divCpChromo :: Int -> Path -> Path -> Vector Path
divCpChromo divPoint (Path papa) (Path mama) =
  let (p1, p2) = splitAt divPoint papa :: (Vector Coord, Vector Coord)
      m1 = filter (`notElem` p1) mama :: Vector Coord
      m2 = filter (`notElem` p2) mama :: Vector Coord
  in Path (p1 ++ m1) `cons` singleton (Path (m2 ++ p2))

shuffleCp :: Int -> Path -> Path -> StdGen -> Vector Path
shuffleCp divPoint (Path papa) (Path mama) gen =
 let (p1, p2) = splitAt divPoint papa :: (Vector Coord, Vector Coord)
     m1 = shuffle (filter (`notElem` p1) mama) gen :: Vector Coord
     m2 = filter (`notElem` p2) mama :: Vector Coord
 in Path (p1 ++ m1) `cons` singleton (Path (m2 ++ p2))