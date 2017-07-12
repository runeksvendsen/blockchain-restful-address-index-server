{-# LANGUAGE ScopedTypeVariables #-}
module Lib.PublishTx.PublishTx
( bitcoindNetworkSumbitTx
, toServantErr
) where

import           Config
import           Lib.Error
import           Network.Bitcoin.AddrIndex.Types
import           Control.Exception
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Network.Bitcoin.Api.Client      (Client, withClient)
import           Network.Bitcoin.Api.Transaction (send)
import           Network.Bitcoin.Api.Misc        (RpcError(..))

import qualified Data.Bitcoin.Transaction        as Btc
import           Network.Haskoin.Transaction     as HT

import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Base16          as B16
import           Data.HexString                  as Hex
import qualified Data.Serialize                  as Bin


bitcoindNetworkSumbitTx :: BTCRPCConf -> HT.Tx -> IO (Either BitcoindErr PushTxResp)
bitcoindNetworkSumbitTx (BTCRPCConf ip port user pass _) tx = fmap PushTxResp <$>
    withClient ip port user pass (bitcoindTry . bitcoindSubmitToNetwork tx)

bitcoindSubmitToNetwork
    :: MonadIO m
    => HT.Tx
    -> Client
    -> m HT.TxHash
bitcoindSubmitToNetwork tx conn = do
    txid <- liftIO $ send conn $ Btc.decode . fromBytes $ serialize tx
    maybe
        (error $ "BUG: Failed to parse transaction ID returned by bitcoind: " ++ show txid)
        return
        (HT.hexToTxHash (B16.encode $ toBytes txid))

serialize :: Bin.Serialize a => a -> BS.ByteString
serialize = Bin.encode

