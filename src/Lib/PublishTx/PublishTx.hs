module Lib.PublishTx.PublishTx
(
    bitcoindNetworkSumbitTx

) where

import           Config

import           Control.Exception               (try)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Network.Bitcoin.Api.Client      (Client, withClient)
import           Network.Bitcoin.Api.Transaction (send)
import           Network.HTTP.Client             (HttpException)

import qualified Data.Bitcoin.Transaction        as Btc
import           Network.Haskoin.Transaction     as HT

import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Base16          as B16
import           Data.HexString                  as Hex
import qualified Data.Serialize                  as Bin



bitcoindNetworkSumbitTx :: BTCRPCConf -> HT.Tx -> IO (Either String HT.TxHash)
bitcoindNetworkSumbitTx (BTCRPCConf ip port user pass _) tx =
    withClient ip port user pass (tryBitcoindSubmitToNetwork tx)

tryBitcoindSubmitToNetwork :: MonadIO m =>
    HT.Tx
    -> Client
    -> m (Either String HT.TxHash)
tryBitcoindSubmitToNetwork tx conn = do
    res <- liftIO . try $ send conn $ Btc.decode . fromBytes $ serialize tx
    case res of
        Left e      -> return . Left $  "Bitcoind error: " ++ show (e :: HttpException)
        Right txid  -> return $
            maybe
                (Left $ "BUG: Failed to parse transaction ID returned by bitcoind: " ++ show txid)
                Right
                (HT.hexToTxHash (B16.encode $ toBytes txid))

serialize :: Bin.Serialize a => a -> BS.ByteString
serialize = Bin.encode

