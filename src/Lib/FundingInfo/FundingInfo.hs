{-# LANGUAGE OverloadedStrings #-}
module Lib.FundingInfo.FundingInfo
(
    module Lib.FundingInfo.FundingInfo
   ,module Lib.FundingInfo.Internal.Types
   ,module Network.Bitcoin.AddrIndex.Types
)

where

import           Util
import           Config                                 (BTCRPCConf (..))
import           Lib.FundingInfo.Internal.Types
import           Network.Bitcoin.AddrIndex.Types

import           Control.Monad                          (mapM)
import           Data.Base58String.Bitcoin              (b58String)
import qualified Data.Base58String.Bitcoin              as B58S
import qualified Data.Bitcoin.Types                     as BT
import           Data.List                              (sortOn)
import qualified Data.Maybe                             as Maybe
import           Network.Bitcoin.Api.Blockchain         (searchRawTransactions)
import           Network.Bitcoin.Api.Client             (Client, withClient)
import           Network.Bitcoin.Api.Types.TxInfo       (TxInfo (..), Vin (..),
                                                         Vout (..))
import qualified Network.Bitcoin.Api.Types.UnspentTxOut as UTXO
import qualified Network.Bitcoin.Api.UTXO               as UTXO
import qualified Network.Haskoin.Crypto                 as HC


match :: UTXO.UnspentTxOut -> AddressFundingInfoRes -> Bool
match
    (UTXO.UnspentTxOut _ utxAmount (utxAddr:_))
    (AddressFundingInfoRes afiAddr _ _ _ afiAmount) =
        afiAddr == utxAddr && afiAmount == utxAmount
match
    (UTXO.UnspentTxOut _ _ [])
    AddressFundingInfoRes {} = False

-- | Return information about all outputs in TxInfo paying to a single address if this
--      address equals the specified address.
outputsPayingToAddress :: B58S.Base58String -> TxInfo -> [AddressFundingInfoRes]
outputsPayingToAddress addr TxInfo{..} =
    map (\vout -> AddressFundingInfoRes addr txid (index vout) confs (amount vout) ) $
        filter ((== addr) . head . addresses) .
        -- An output can pay to more than one address, apparently. We ignore these outputs.
        filter ((== 1) . length . addresses) $ vouts

-- |If the given 'AddressFundingInfoRes' is redeemed by the given 'TxInfo', return
--  "Just (txid txInfo)", otherwise return Nothing.
redeemedBy :: AddressFundingInfoRes -> TxInfo -> Maybe BT.TransactionId
redeemedBy (AddressFundingInfoRes _ afiTxId afiIdx _ _)
            TxInfo{ txid = redeemingTxId, vins = vins } =
    if any (== True) (map doesVinRedeemOutput vins) then Just redeemingTxId else Nothing
        where doesVinRedeemOutput (Vin tid idx) = tid == afiTxId && idx == afiIdx
              doesVinRedeemOutput CoinbaseVin   = False

-- |Check whether a given output has been spent and, while we're at it, confirm that the
--  Bitcore Core address
--  index and transaction index agree with each other.
--  Returns (Just unspentOutput) if the output is unspent,
--  or Nothing if the given output has been spent, and fails horribly if there's
--  a disagreement between the indices (which would indicate a bug).
checkSpentAndConfirmData :: Client -> AddressFundingInfoRes -> IO (Maybe AddressFundingInfoRes)
checkSpentAndConfirmData client afi@(AddressFundingInfoRes _ txid index _ _) =
    -- 'UTXO.getTxOut _ _ _ False' = ignore unconfirmed spending transactions.
    --  If set to False, outputs are not removed from the list of unspent outputs
    --  until the spending tx has at least a single confirmation.
    fmap confirmMatchOrFail <$> UTXO.getTxOut client txid index True
    where
        confirmMatchOrFail :: UTXO.UnspentTxOut -> AddressFundingInfoRes
        confirmMatchOrFail utxOut  =
            if utxOut `match` afi then
                    afi
                else
                    error $ "BUG: Address index/tx index discrepancy: "
                          ++ show (utxOut, afi)

-- | Get all outputs related to an address
outsForAddr :: BTCRPCConf -> B58S.Base58String -> IO [AddressFundingInfoRes]
outsForAddr rpcConf addr =
    withClient' rpcConf $ \client -> do
        outs <- sortOn timestamp <$> searchRawTransactions client addr
        let payingOuts = concatMap (outputsPayingToAddress addr) (outs :: [TxInfo])


        return undefined




getAllOutputs' :: BTCRPCConf -> B58S.Base58String -> IO [AddressFundingInfoRes]
getAllOutputs' rpcConf addr =
    withClient' rpcConf $ \client ->
        concatMap (outputsPayingToAddress addr) .
        reverse . sortOn timestamp <$>
            searchRawTransactions client addr

-- |Get all outputs in the Blockchain paying to address
getAllOutputs :: BTCRPCConf -> HC.Address -> IO [AddressFundingInfo]
getAllOutputs conf addr = map toHaskoin <$>
    getAllOutputs' conf addr'
  where
    addr' = b58String . HC.addrToBase58 $ addr

-- |Get all unredeemed outputs in the Blockchain paying to address
getUnredeemedOutputs :: BTCRPCConf -> HC.Address -> IO [AddressFundingInfo]
getUnredeemedOutputs conf addr = map toHaskoin <$>
    withClient' conf getAllCheck
  where
    getAllCheck client =
          fmap Maybe.catMaybes $
              getAllOutputs' conf addr' >>=
              mapM (checkSpentAndConfirmData client)
    addr' = b58String . HC.addrToBase58 $ addr



withClient' :: BTCRPCConf -> (Client -> IO a) -> IO a
withClient' (BTCRPCConf host port user pass _) =
    withClient host port user pass
