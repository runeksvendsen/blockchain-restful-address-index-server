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
)
where


import           Control.Exception       (try)
import           Control.Monad           (forM)
import qualified Control.Monad.Reader    as Reader
import           Data.EitherR            (fmapL)
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import           Data.String.Conversions (cs)

import qualified Network.Haskoin.Util    as HU

decodeHex bs = maybe (Left "invalid hex string") Right (HU.decodeHex bs)
