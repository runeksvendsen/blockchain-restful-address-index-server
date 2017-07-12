module Types
( module Types
, Client
, Conf.BTCRPCConf(..)
, Reader.MonadIO
)
where

import qualified Config               as Conf
import qualified Control.Monad.Reader as Reader
import           Servant
import           Network.Bitcoin.Api.Client             (Client, withClient)


-- Enables our handlers to pull a 'BTCRPCConf' out of thin air
type AppM = Reader.ReaderT Conf.BTCRPCConf Handler
