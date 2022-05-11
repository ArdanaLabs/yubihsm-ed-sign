{-# LANGUAGE ScopedTypeVariables #-}
import Lib

import Control.Exception
import qualified Data.ByteString as B
import Data.Text.Encoding
import Data.Text (pack)

testKeyID = 200;
main :: IO ()
main = do
  testPutEdKey
  testSignWithEdKey
  putStrLn("finished");

-- | Use this to test if the Rust library is linked propertly.
--
-- Expect: "key length cannot be anything else than 32 characters"
testPutEdKey :: IO ()
testPutEdKey = do
 res <- putEdKey 
    (Id testKeyID) 
    (Label (encodeUtf8 $ pack "foo")) 
    (Domains 1)
    secretKey 
    True
 let res' = assert (res == True) ("test passed " <> (show $ res == True))
 putStrLn("result " <> show res')
 putStrLn("res is " <> show res)
 
testSignWithEdKey :: IO ()
testSignWithEdKey = do

 sig :: B.ByteString <- signWithEdKey
                          (Id testKeyID)
                          message
                          True
 let emptyTest = if (B.null sig) then "sig is empty" else "sig not empty"
 let res = if sig == signature  then "success" else "failed"
 putStrLn ("result is " <> res)
 putStrLn emptyTest
 


       