module Network.Factory where

import           Control.Monad       (mapM)
import qualified Data.AndLogic       as And
import qualified Data.Car            as Car
import qualified Data.Iris           as Iri
import qualified Data.WDBC           as Wbdc
import qualified Data.Wine           as Wi
import           Network
import           Network.AutoEncoder
import           Network.Helpers
import           Network.MlpEncoder
import           System.Random       (StdGen)
import qualified System.Random       as Rand

irisOptimal = kFoldFac (MlpConfig False [7, 5, 3] 4 []) 100 13 Iri.data'

irisRange = (\c -> kFoldFac c 20 7 Iri.data') `mapM` configGen False 4 3 3 10

wineOptimal = kFoldFac (MlpConfig False [13, 7, 3] 13 []) 100 3 Wi.data'

wineRange = (\c -> kFoldFac c 20 7 Wi.data') `mapM` configGen False 13 3 5 15

carOptimal = kFoldFac (MlpConfig False [18, 8, 4] 6 []) 20 3 Car.data'

carRange = (\c -> kFoldFac c 20 3 Car.data') `mapM` configGen False 6 4 5 10

wdbcOptimal = kFoldFac (MlpConfig True [14, 7, 4, 2] 30 []) 20 7 Wbdc.data'

wdbcRange = (\c -> kFoldFac c 20 7 Wbdc.data') `mapM` configGen True 30 2 4 7

andLogicOptimal = andLogicFac (MlpConfig True [7, 4, 3, 2] 2 []) 1000 1

carEncOptimal =
  let mlpConf = MlpConfig False [10, 8, 4] 9 []
      aeConf = AutoEncoderConfig [5, 3, 5] 6
      cConns = replicate 4 [-1, -2, -3, -4, -5, -6] ++ replicate 6 [-7, -8, -9]
   in kFoldWithEncoderFac (MlpEnConfig mlpConf aeConf cConns) 20 3 Car.data'

irisEncOptimal =
  let mlpConf = MlpConfig False [7, 5, 3] 6 []
      aeConf = AutoEncoderConfig [3, 2, 3] 4
      cConns = replicate 5 [-1, -2, -3, -4] ++ replicate 3 [-5, -6]
   in kFoldWithEncoderFac (MlpEnConfig mlpConf aeConf cConns) 100 10 Iri.data'

wineEncOptimal =
  let mlpConf = MlpConfig False [13, 7, 3] 17 []
      aeConf = AutoEncoderConfig [7, 4, 7] 13
      cConns = replicate 5 [-1, -2, -3, -4, -5, -7, -8, -9, -10, -11, -12, -13] ++ replicate 8 [-14, -15, -16, -17]
   in kFoldWithEncoderFac (MlpEnConfig mlpConf aeConf cConns) 100 7 Wi.data'

-- FACTORIES
kFoldFac :: MlpConfig -> Int -> Int -> IO [([Double], [Double])] -> IO [(Double, Mlp)]
kFoldFac conf epoch k data' = do
  g <- Rand.newStdGen
  net <- new g $ createAllToAllConnections conf
  map (learnAndTest net) . kFold g k . minMaxScaling <$> data'
  where
    addEpoch train = concat $ replicate epoch train
    learnAndTest net (train, test) = tester test $ learn (addEpoch train) net

kFoldWithEncoderFac :: MlpEnConfig -> Int -> Int -> IO [([Double], [Double])] -> IO [(Double, MlpEncoder)]
kFoldWithEncoderFac (MlpEnConfig mlpC aeC customCons) epoch k data' = do
  g <- Rand.newStdGen
  uData <- data'
  readyEnc <- unwrapEncoder . enStruct . learn (addEpoch uData) <$> newEnc g aeC
  net <- new g finalCons
  let mlpEnc = MlpEncoder net readyEnc
  return $ map (learnAndTest mlpEnc) . kFold g k . minMaxScaling $ uData
  where
    finalCons = replaceConns 0 customCons $ createAllToAllConnections mlpC
    addEpoch train = concat $ replicate epoch train
    learnAndTest net (train, test) = tester test $ learn (addEpoch train) net

andLogicFac :: MlpConfig -> Int -> Int -> IO (Double, Mlp)
andLogicFac conf epoch genNetNum = do
  let cons = createAllToAllConnections conf
  learningData <- concat . replicate epoch <$> And.learningData
  testData <- And.testData
  findBestNet genNetNum cons learningData testData
