module Data.AndLogic
  ( learningData
  , testData
  ) where

data' :: [([Double], [Double])]
data' = [([0, 1], [0, 1]), ([1, 1], [1, 0]), ([0, 1], [0, 1]), ([1, 0], [0, 1]), ([0, 0], [0, 1])]

learningData :: IO [([Double], [Double])]
learningData = return data' --return [([1, 1], [1, 0]), ([0, 0], [0, 1])]

testData :: IO [([Double], [Double])]
testData = return data'
