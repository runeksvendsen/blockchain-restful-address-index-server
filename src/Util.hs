module Util
(
  fromMaybe
, forM
, cs
, try
, fmapL
, (<>)
, Reader.ask, Reader.liftIO
, decodeHex
, module X
)
where

import Types as X

import Config

import           Control.Exception       (try)
import           Control.Monad           (forM)
import qualified Control.Monad.Reader    as Reader
import           Data.EitherR            (fmapL)
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import           Data.String.Conversions (cs)
import qualified Data.ByteString                 as BS

import qualified Network.Haskoin.Util    as HU


decodeHex :: BS.ByteString -> Either String BS.ByteString
decodeHex bs = maybe (Left "invalid hex string") Right (HU.decodeHex bs)

