module Lib where

import Foreign.C.Types (CUShort(..), CSize(..))
import Foreign.C.String (CString)
import qualified Foreign.Marshal.Alloc as A
import Data.Word (Word16)
import Control.Monad (unless)
import qualified Data.ByteString as B


foreign import ccall unsafe "put_ed_key" put_ed_key
  :: CUShort -> CString -> CUShort -> CString -> IO ()

foreign import ccall unsafe "sign_with_ed_key" sign_with_ed_key
  :: CUShort -> CString -> CSize -> CString -> IO ()

newtype Id = Id Word16 deriving (Eq, Show)
newtype Label = Label B.ByteString deriving (Eq, Show)
newtype Domains = Domains Word16 deriving (Eq, Show)

labelSize :: Int
labelSize = 40

keySize :: Int
keySize = 64

signatureSize :: Int
signatureSize = 64

putEdKey :: Id -> Label -> Domains -> B.ByteString -> IO ()
putEdKey (Id i) (Label label) (Domains d) key = do
  let labelLen = B.length label
  unless (labelLen <= 40) (error "label cannot be longer than 40 characters")
  unless (B.length key == keySize)
    (error "key length cannot be anything else than 40 characters")
  let paddedLabel = label `B.append` B.replicate (labelSize - labelLen) 0
  B.useAsCString paddedLabel $ \lb ->
    B.useAsCString key $ \k ->
      put_ed_key (CUShort i) lb (CUShort d) k

signWithEdKey :: Id -> B.ByteString -> IO B.ByteString
signWithEdKey (Id i) messageB =
  A.allocaBytes signatureSize $ \outputBuffer -> do
    B.useAsCStringLen messageB $ \(msgptr, msglen) ->
      sign_with_ed_key (CUShort i) msgptr (fromIntegral msglen) outputBuffer
    B.packCStringLen (outputBuffer, signatureSize)
