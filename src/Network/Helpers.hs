module Network.Helpers
  ( resAll
  , tester
  , minMaxScaling
  , kFold
  , kFoldSummary
  , printNet
  , configGen
  , getResult
  , replaceConns
  , findBestNet
  , createAllToAllConnections
  , splits
  ) where

import           Control.Monad             (forM, forM_, zipWithM, zipWithM_)
import           Data.Aeson
import qualified Data.ByteString.Lazy      as BS
import           Data.List
import           Data.List.Split           (chunksOf)
import           Data.Maybe
import           Data.Ord
import qualified Data.Vector               as Vec
import           Debug.Trace
import           Network
import           System.Random             (StdGen)
import qualified System.Random             as Rand
import           VectorShuffling.Immutable

configGen :: Bool -> Int -> Int -> Int -> Int -> [MlpConfig]
configGen bias i o maxHiddenL maxP = map (\l -> MlpConfig bias l i []) layers
  where
    layers = concat [gn x maxP [o] | x <- [0 .. maxHiddenL]]

gn :: Int -> Int -> [Int] -> [[Int]]
gn 0 _ prev = [prev]
gn n pMax prev = concat [gn (n - 1) pMax (x : prev) | x <- [head prev + 1 .. pMax]]

createAllToAllConnections :: MlpConfig -> MlpConfig
createAllToAllConnections conf@(MlpConfig True dims inputsNum _) =
  makeRes $
  foldl
    (\(index, conns, res) size -> (index + size + 1, [index .. index + size], ([] : replicate size conns) : res))
    (1, [-inputsNum .. 0], [])
    dims
  where
    makeRes (_, _, res) = conf {confConnections = [] : concat (reverse res)}
createAllToAllConnections conf@(MlpConfig False dims inputsNum _) =
  makeRes $
  foldl
    (\(index, conns, res) size -> (index + size, [index .. index + size - 1], replicate size conns : res))
    (0, [-inputsNum .. (-1)], [])
    dims
  where
    makeRes (_, _, res) = conf {confConnections = concat (reverse res)}

replaceConns :: Int -> [NeuronConnections] -> MlpConfig -> MlpConfig
replaceConns startIndex conns conf = conf {confConnections = prefix ++ conns ++ suffix}
  where
    prefix = take startIndex (confConnections conf)
    suffix = drop (startIndex + length conns) (confConnections conf)

--printNet :: Mlp -> IO ()
--printNet (Mlp _ _ arr) = mapM_ print arr
resAll :: [([Double], [Double])] -> Mlp -> IO ()
resAll data' mlp = mapM_ (\(i, o) -> print (getResult (forward i mlp))) data'

tester :: (Network net) => [([Double], [Double])] -> net -> (Double, net)
tester data' mlp
  -- traceShow (show (length data')) $
 = makeRes $ sum $ map (\(i, destOutput) -> compare destOutput (getResult (forward i mlp))) data'
  where
    makeRes s = ((s / fromIntegral (length data')) * 100, mlp)
    compare dest res
      --traceShow ("dest: " ++ show dest ++ "  |  res: " ++ show res) $
     =
      if snd (maxi dest) == snd (maxi res)
        then 1
        else 0

minMaxScaling :: [([Double], [Double])] -> [([Double], [Double])]
minMaxScaling d = zipWith (\n (_, c) -> (n, c)) (scale (map fst d)) d
  where
    scale inputs =
      let maxMin = findMaxMin inputs
       in map (zipWith (\(max', min') val -> (val - min') / (max' - min')) maxMin) inputs

kFoldSummary :: (Network net) => Bool -> [(Double, net)] -> IO ()
kFoldSummary details l = do
  printNet $ snd $ head l
  if details
    then zipWithM_ (\i (v, _) -> putStrLn ("K=" ++ show i ++ " accuracy: " ++ show v)) [1 ..] l
    else return ()
  putStrLn $ "Summary accuracy: " ++ show sumUpRes
  where
    sumUpRes = sum (map fst l) / len
    len = fromIntegral $ length l

kFold :: StdGen -> Int -> [([Double], [Double])] -> [([([Double], [Double])], [([Double], [Double])])]
kFold g k d = map prepareFold [0 .. k - 1]
  where
    prepareFold i = (\(train, test) -> (concat train, test)) (remove i split')
    split' = map (\x -> Vec.toList (fst (shuffle (Vec.fromList x) g))) (splits k d)

-- PRIVATES
findMaxMin :: [[Double]] -> [(Double, Double)]
findMaxMin d = foldl (zipWith find) initAcc d
  where
    initAcc = map (\x -> (x, x)) $ head d
    find (max', min') val
      | val > max' = (val, min')
      | val < min' = (max', val)
      | otherwise = (max', min')

--    HINT classes should occurs after each other without mixing
splits :: Int -> [([Double], [Double])] -> [[([Double], [Double])]]
splits k d = foldl (\acc x -> zipWith (++) (chunksOf (num x) x) acc) initAcc grouped
  where
    initAcc = replicate k []
    num x = ceiling $ fromIntegral (length x) / fromIntegral k
    grouped = groupBy (\(_, b) (_, b') -> b == b') d

remove :: Int -> [a] -> ([a], a)
--TODO is somewhere a bug. it caused that when k is relatively high, data are split into not enough folds
remove i items --traceShow (show (length items))
 = (take i items ++ drop (1 + i) items, items !! i)

maxi xs = maximumBy (comparing fst) (zip xs [0 ..])

-- DEPRECATED
--findBestNet :: Int -> -> IO ()
findBestNet i cons learnData testData =
  fmap
    findBest
    (forM [1 .. i] $ \_ -> do
       g <- Rand.newStdGen
       net <- new g cons
       let learnedNet = learn learnData net
       return $ tester testData learnedNet)
  where
    findBest = maximumBy (comparing fst)
