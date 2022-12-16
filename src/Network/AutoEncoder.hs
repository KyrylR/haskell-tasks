module Network.AutoEncoder
  ( newEnc
  , AutoEncoderConfig(..)
  , AEnc(..)
  , unwrapEncoder
  ) where

import qualified Data.Vector     as Vec
import           Network
import           Network.Helpers (createAllToAllConnections)
import           System.Random   (StdGen)

data AutoEncoderConfig = AutoEncoderConfig
  { enConfHidden       :: [Int]
  , ecConfInputsNumber :: Int
  }

data AEnc = AEnc
  { enType   :: Type
  , enStruct :: Mlp
  }

data Type
  = EncoderDecoder
  | Encoder
  | Decoder

instance Network AEnc where
  forward d t@(AEnc type' mlp) = AEnc type' $ forward d mlp
  backpropagate desO t@(AEnc type' mlp) = AEnc type' $ backpropagate desO mlp
  learn d t@(AEnc type' mlp) = AEnc type' $ learn (map (\(x, _) -> (x, x)) d) mlp
  getResult (AEnc _ mlp) = getResult mlp
  printNet (AEnc _ mlp) = printNet mlp

newEnc :: StdGen -> AutoEncoderConfig -> IO AEnc
newEnc stdGen conf@(AutoEncoderConfig hid inputsNum) = AEnc EncoderDecoder <$> new stdGen toMlpConfig
  where
    toMlpConfig = createAllToAllConnections $ MlpConfig False layers inputsNum []
    layers = (inputsNum : hid) ++ [inputsNum]

unwrapEncoder :: Mlp -> Mlp
unwrapEncoder (Mlp dims _ neurons) = Mlp newDims [] newNeurons
  where
    newDims = take (ceiling (fromIntegral (length dims) / 2)) dims
    newNeurons = Vec.take (sum newDims) neurons

unwrapDecoder :: Mlp -> Mlp
unwrapDecoder (Mlp dims _ neurons) = Mlp newDims [] newNeurons
  where
    newDims = drop (ceiling (fromIntegral (length dims) / 2) - 1) dims
    dropSum = sum $ take (ceiling (fromIntegral (length dims) / 2)) dims
    newNeurons = Vec.drop dropSum neurons
