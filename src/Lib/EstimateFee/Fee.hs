module Lib.EstimateFee.Fee where

import Types
import Lib.Error
import Network.Bitcoin.Api.Misc                 (estimateFee)
--import qualified Data.Bitcoin.Types        as Btc
--import Data.Word (Word64)
import           Data.Fixed                      (Fixed (MkFixed))

-- | Get fee needed to have a transaction confirmed within "maxBlocks"
--    in satoshis per byte
getEstimatedFee :: Word -> AppM (Either BitcoindErr Word)
getEstimatedFee maxBlocks =
    withClientSafe (getEstimatedFeeUnsafe maxBlocks)

getEstimatedFeeUnsafe :: Word -> Client -> IO Word
getEstimatedFeeUnsafe maxBlocks client = do
    satoshisPerKb <- estimateFee client (fromIntegral maxBlocks)
    return . fromIntegral . getFixed $ satoshisPerKb / 10^3
  where
    getFixed (MkFixed a) = a

