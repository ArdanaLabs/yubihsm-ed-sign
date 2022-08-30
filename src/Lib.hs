module Lib
  (helloWorld
  ,publicKey
  ,secretKey
  ,putEdKey
  ,signature
  ,signWithEdKey
  ,getPubKey
  ,message
  ,Id(Id)
  ,Label(Label)
  ,Domains(Domains)
  ) where

import Foreign.C.Types (CUShort(..), CSize(..), CBool (CBool))
import Foreign.C.String (CString)
import qualified Foreign.Marshal.Alloc as A
import Data.Word (Word16, Word8)
import Control.Monad (unless)
import qualified Data.ByteString as B

foreign import ccall unsafe "put_ed_key" put_ed_key
  :: CUShort -> CString -> CUShort -> CString -> CBool -> IO Bool

foreign import ccall unsafe "sign_with_ed_key" sign_with_ed_key
  :: CUShort -> CString -> CSize -> CString -> CBool -> IO ()

foreign import ccall unsafe "hello_world" hello_world
  :: IO ()

foreign import ccall unsafe "get_public_key" get_public_key
  ::  CUShort -> CString -> CBool -> IO ()

newtype Id = Id Word16 deriving stock (Eq, Show)
newtype Label = Label B.ByteString deriving stock (Eq, Show)
newtype Domains = Domains Word16 deriving stock (Eq, Show)

labelSize :: Int
labelSize = 40

keySize :: Int
keySize = 32

signatureSize :: Int
signatureSize = 64

helloWorld :: IO()
helloWorld = hello_world

signature :: B.ByteString
signature = "\xE5\x56\x43\x00\xC3\x60\xAC\x72\x90\x86\xE2\xCC\x80\x6E\x82\x8A\x84\x87\x7F\x1E\xB8\xE5\xD9\x74\xD8\x73\xE0\x65\x22\x49\x01\x55\x5F\xB8\x82\x15\x90\xA3\x3B\xAC\xC6\x1E\x39\x70\x1C\xF9\xB4\x6B\xD2\x5B\xF5\xF0\x59\x5B\xBE\x24\x65\x51\x41\x43\x8E\x7A\x10\x0B"

publicKey :: B.ByteString
publicKey = "\xD7\x5A\x98\x01\x82\xB1\x0A\xB7\xD5\x4B\xFE\xD3\xC9\x64\x07\x3A\x0E\xE1\x72\xF3\xDA\xA6\x23\x25\xAF\x02\x1A\x68\xF7\x07\x51\x1A"
secretKey :: B.ByteString
secretKey = "\x9D\x61\xB1\x9D\xEF\xFD\x5A\x60\xBA\x84\x4A\xF4\x92\xEC\x2C\xC4\x44\x49\xC5\x69\x7B\x32\x69\x19\x70\x3B\xAC\x03\x1C\xAE\x7F\x60"

message :: B.ByteString
message = B.empty

putEdKey :: Id -> Label -> Domains -> B.ByteString -> Bool -> IO Bool
putEdKey (Id i) (Label label) (Domains d) key isTesting = do

  let labelLen = B.length label
  unless (labelLen <= 40) (error "label cannot be longer than 40 characters")
  unless (B.length key == keySize)
    (error "key length cannot be anything else than 32 characters")
  let paddedLabel = label `B.append` B.replicate (labelSize - labelLen) 0
  let isTestingWord :: Word8
      isTestingWord = fromInteger (toInteger (fromEnum isTesting)) :: Word8
  B.useAsCString paddedLabel $ \lb ->
    B.useAsCString key $ \k ->
      put_ed_key (CUShort i) lb (CUShort d) k (CBool isTestingWord)

signWithEdKey :: Id -> B.ByteString -> Bool -> IO B.ByteString
signWithEdKey (Id i) messageB isTesting =
  A.allocaBytes signatureSize $ \outputBuffer -> do
    let isTestingWord :: Word8
        isTestingWord = fromInteger (toInteger (fromEnum isTesting))
    B.useAsCStringLen messageB $ \(msgptr, msglen) ->
      sign_with_ed_key (CUShort i) msgptr (fromIntegral msglen) outputBuffer (CBool isTestingWord)

    B.packCStringLen (outputBuffer, signatureSize)

getPubKey :: Id -> Bool -> IO B.ByteString
getPubKey (Id i) isTesting =
  A.allocaBytes keySize $ \outputBuffer -> do
    let isTestingWord :: Word8
        isTestingWord = fromInteger (toInteger (fromEnum isTesting))
    get_public_key (CUShort i) outputBuffer (CBool isTestingWord)
    B.packCStringLen (outputBuffer, keySize)


