module TSP (
  totalCost
, Path(..)
, Coord(..)
)where

import Data.Vector
import Prelude hiding (
  enumFromTo,
  length,
  last,
  head,
  sum
  )

newtype Coord = Coord (Double, Double) deriving (Eq, Show, Ord)
newtype Path = Path (Vector Coord) deriving (Eq, Show)

instance Ord Path where
  compare p1 p2
    | totalCost p1 > totalCost p2 = GT
    | totalCost p1 == totalCost p2 = EQ
    | totalCost p1 < totalCost p2 = LT

eachTuple :: Vector Coord -> Vector (Coord, Coord)
eachTuple path =
  let tuples = do
        index <- enumFromTo 0 (length path - 2)
        return (path ! index, path ! (index + 1))
  in tuples `snoc` (last path, head path)

totalCost :: Path -> Double
totalCost (Path path) = sum $ uncurry distance <$> eachTuple path

distance :: Coord -> Coord -> Double
distance (Coord (x1, y1)) (Coord (x2, y2)) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2
