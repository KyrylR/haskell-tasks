module Shuffle (
  shufflePath,
  shuffle,
  shuffleM
) where

import TSP
import System.Random
import Data.Vector
import Prelude hiding (head, tail, length)

shufflePath :: Path -> StdGen -> Path
shufflePath (Path coords) gen = Path $ shuffle coords gen

shuffle :: (Eq a) => Vector a -> StdGen -> Vector a
shuffle x gen
  | x == empty = empty
  | otherwise  =
      let (index,newGen) = randomR (0, length x - 1) gen :: (Int, StdGen)
          targetElm      = x ! index
      in targetElm `cons` shuffle (deleteFirst targetElm x) newGen

shuffleM :: (Eq a) => Vector a -> IO (Vector a)
shuffleM x = do
  gen <- newStdGen
  return $ shuffle x gen

-- | 削除したい要素の中で最初に出てきたものを取り除く
-- >>> deleteFirst 2 $ fromList [1..5]
-- [1,3,4,5]
-- >>> deleteFirst 1 $ fromList [1,2,3,4,1]
-- [2,3,4,1]
deleteFirst :: (Eq a) => a -> Vector a -> Vector a
deleteFirst x vector
  | x == head vector = tail vector
  | otherwise        = head vector `cons` deleteFirst x (tail vector)
