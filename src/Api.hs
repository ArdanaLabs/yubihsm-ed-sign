module Api(getPK,signTx) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString.Bech32(encodeBech32,HumanReadablePart(HumanReadablePart))
import Data.Text(unpack)
import Lib (signWithEdKey, Id (Id),getPubKey)
import Hex(fromHex)

-- | Takes a serialised TX as a hex string
-- and produces a Bech32 encoded signature
signTx :: String -> IO String
signTx w = do
  let bytes = fromHex w
  signed <- liftIO $ signWithEdKey (Id 200) bytes True
  return $ unpack $ encodeBech32 (HumanReadablePart "ed25519_sig") signed
  -- got this magic string from tracing valid txs in ctl

-- | produces a Bech32 encoded publicKey hash
getPK :: IO String
getPK = do
  pk <- liftIO $ getPubKey (Id 200) True
  return $ unpack $ encodeBech32 (HumanReadablePart "ed25519_pk") pk
  -- Got this magic string from csl source code
  -- https://github.com/Emurgo/cardano-serialization-lib/blob/e4ae8728a79f77a25c70026bb4fdd4d567a9a20e/rust/src/chain_crypto/algorithms/ed25519.rs#L54

