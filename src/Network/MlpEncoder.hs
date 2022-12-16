module Network.MlpEncoder where

import           Debug.Trace
import           Network
import qualified Network.AutoEncoder as AE
import           Network.Helpers     (getResult)
import           System.Random       (StdGen)

data MlpEnConfig = MlpEnConfig
  { mlpConfig   :: MlpConfig
  , aeConfig    :: AE.AutoEncoderConfig
  , customConns :: [NeuronConnections] --only first layer
  }

data MlpEncoder = MlpEncoder
  { mlp :: Mlp
  , ae  :: Mlp
  }

instance Network MlpEncoder where
  backpropagate desOut (MlpEncoder mlp ae) = MlpEncoder (backpropagate desOut mlp) ae
  learn data' net = foldl (\net' (inputs, desireOutputs) -> backpropagate desireOutputs (forward inputs net')) net data'
  forward data' (MlpEncoder mlp ae) = MlpEncoder (forward mlpInput mlp) ae
    where
      mlpInput = data' ++ getResult (forward data' ae)
  getResult (MlpEncoder mlp _) = getResult mlp
  printNet (MlpEncoder mlp _) = printNet mlp
