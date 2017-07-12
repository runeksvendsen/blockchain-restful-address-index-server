{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Error
( BitcoindErr(..)
, HttpException
, toServantErr
, bitcoindTry
, withClientSafe
, nothing404
) where

import           Util
import           Network.Bitcoin.Api.Misc        (RpcError(..))
import qualified Data.Aeson                      as JSON
import           Servant.Server
import           Network.HTTP.Client             (HttpException)
import           Control.Exception
import           Network.Bitcoin.Api.Client             (Client, withClient)


instance JSON.ToJSON RpcError where
    toJSON RpcError{..} = JSON.object
        [ ("err_code", JSON.toJSON errCode)
        , ("err_msg" , JSON.toJSON errMsg)
        ]

data BitcoindErr
  = BitcoindErr RpcError
  | ConnectionErr HttpException
  | NotFound

nothing404 :: forall b. Either BitcoindErr (Maybe b) -> Either BitcoindErr b
nothing404 (Right resM) = maybe (Left NotFound) Right resM
nothing404 (Left e) = Left e


toServantErr :: BitcoindErr -> ServantErr
toServantErr (BitcoindErr err) = err400 { errBody = JSON.encode err }
toServantErr (ConnectionErr ex) = error $ "bitcoind connection error: " ++ show ex
toServantErr NotFound = err404 { errBody = "Transaction(s) not found" }

withClientSafe :: (Client -> IO a) -> AppM (Either BitcoindErr a)
withClientSafe f = do
    (BTCRPCConf host port user pass _) <- ask
    liftIO $ bitcoindTry $ withClient host port user pass f


bitcoindTry
    :: MonadIO m
    => IO a
    -> m (Either BitcoindErr a)
bitcoindTry action = liftIO $
    (Right <$> action) `catches`
        [ Handler $ \(ex :: HttpException) ->
              return . Left $ ConnectionErr ex
        , Handler $ \(ex :: RpcError)      ->
              return . Left $ BitcoindErr ex
        ]

