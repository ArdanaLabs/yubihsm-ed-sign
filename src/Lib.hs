{-# LANGUAGE OverloadedStrings #-}
module Lib where

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

newtype Id = Id Word16 deriving (Eq, Show)
newtype Label = Label B.ByteString deriving (Eq, Show)
newtype Domains = Domains Word16 deriving (Eq, Show)

labelSize :: Int
labelSize = 40

keySize :: Int
keySize = 32

signatureSize :: Int
signatureSize = 64

helloWorld :: IO() 
helloWorld = hello_world

publicKey :: B.ByteString
publicKey = "\xD7\x5A\x98\x01\x82\xB1\x0A\xB7\xD5\x4B\xFE\xD3\xC9\x64\x07\x3A\x0E\xE1\x72\xF3\xDA\xA6\x23\x25\xAF\x02\x1A\x68\xF7\x07\x51\x1A"

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
