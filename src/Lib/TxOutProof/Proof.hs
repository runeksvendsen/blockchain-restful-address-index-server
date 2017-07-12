{-# LANGUAGE OverloadedStrings #-}
module Lib.TxOutProof.Proof where

import Lib.Error
import qualified Data.Aeson                           as JSON
import qualified Data.Bitcoin.Types                   as BT
import           Network.Bitcoin.AddrIndex.Types
import           Network.Bitcoin.Api.Blockchain       as Chain
import qualified Network.Bitcoin.Api.UTXO             as UTXO
import           Types
import           Util

import qualified Control.Exception                    as E
import           Data.HexString                       as Hex
import qualified Data.Serialize                       as Bin
import qualified Data.Text                            as T
import qualified Network.Haskoin.Block                as HB
import qualified Network.Haskoin.Transaction          as HT
import           Network.HTTP.Client                  (HttpException)
import           Servant


-- | Get funding proof for given txid.
getFundingProof :: HT.TxHash -> AppM (Either BitcoindErr FundingProof)
getFundingProof txid' = do
    txStrE <- nothing404 <$> withClientSafe (`Chain.getRawTransaction` txid)
    either (return . Left) createProof txStrE
  where
    createProof txStr = do
        let tx = errConv . Bin.decode . Hex.toBytes $ txStr
        proofE <- getProof [txid] Nothing
        return $ FundingProof tx <$> proofE
    txid = convOrFail txid'
    errConv = either (\str -> E.throw $ err500 { errBody = "JSON conversion error: " <> cs str }) id

convOrFail :: (Show a, JSON.ToJSON a, JSON.FromJSON b) => a -> b
convOrFail a = fromMaybe (failErr a) (JSON.decode . JSON.encode $ a)
    where failErr = error . ("JSON conversion error. Source: " ++) . show


getProof :: [BT.TransactionId] -> Maybe BT.BlockHash -> AppM (Either BitcoindErr HB.MerkleBlock)
getProof txIdL mB =
    nothing404 <$> withClientSafe (\client -> getProofUnsafe client txIdL mB)

parseTxIds :: [T.Text] -> [BT.TransactionId]
parseTxIds = map $ hexString . cs

getProofUnsafe :: Client -> [BT.TransactionId] -> Maybe BT.BlockHash -> IO (Maybe HB.MerkleBlock)
getProofUnsafe client txIdL bhM =
    fmap decodeHex <$> UTXO.txOutProof client txIdL bhM
  where
    decodeHex hex = case Bin.decode $ Hex.toBytes hex of
          Right mb -> mb
          Left   e -> error $
              "Couldn't decode MerkleBlock from hex data: " ++ show e ++
              "\n" ++ show hex
