{-# LANGUAGE ScopedTypeVariables #-}
module Lib.PublishTx.PublishTx
( bitcoindNetworkSumbitTx
, toServantErr
) where

import           Config

import           Control.Exception
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Network.Bitcoin.Api.Client      (Client, withClient)
import           Network.Bitcoin.Api.Transaction (send)
import           Network.Bitcoin.Api.Misc        (RpcError(..))
import           Network.HTTP.Client             (HttpException)

import qualified Data.Bitcoin.Transaction        as Btc
import           Network.Haskoin.Transaction     as HT

import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Base16          as B16
import           Data.HexString                  as Hex
import qualified Data.Serialize                  as Bin
import qualified Data.Aeson                      as JSON
import           Servant.Server


instance JSON.ToJSON RpcError where
    -- RpcError { errCode :: Int, errMsg :: T.Text }
    toJSON RpcError{..} = JSON.object
        [ ("err_code", JSON.toJSON errCode)
        , ("err_msg" , JSON.toJSON errMsg)
        ]

bitcoindNetworkSumbitTx :: BTCRPCConf -> HT.Tx -> IO (Either BitcoindErr HT.TxHash)
bitcoindNetworkSumbitTx (BTCRPCConf ip port user pass _) tx =
    withClient ip port user pass (tryBitcoindSubmitToNetwork tx)

data BitcoindErr
  = BitcoindErr RpcError

toServantErr :: BitcoindErr -> ServantErr
toServantErr (BitcoindErr err) = err400 { errBody = JSON.encode err }

tryBitcoindSubmitToNetwork :: Tx -> Client -> IO (Either BitcoindErr TxHash)
tryBitcoindSubmitToNetwork tx c =
    (Right <$> bitcoindSubmitToNetwork tx c) `catches`
        [ Handler $ \(ex :: HttpException) -> error $ "Bitcoind error: " ++ show (ex :: HttpException)
        , Handler $ \(ex :: RpcError)      ->
              return . Left $ BitcoindErr (ex :: RpcError)
        ]

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

--
-- 63
-- 2103419c40ee7543c79397fa254bbab33a50ce30e7495759fb5af117c29e9c960b24
-- ad6704943b115ab17568
-- 2103ec4b4f1bc9afe6a742f9b1d0ca8b5ada2eb8631a6a9062b734ca67e6db77eb7d
-- ac
