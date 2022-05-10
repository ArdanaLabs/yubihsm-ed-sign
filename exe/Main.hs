import Lib

import Control.Exception
import Data.Text.Encoding
import Data.Text (pack)

testKeyID = 200;
main :: IO ()
main = do
  test
  putStrLn("finished");

-- | Use this to test if the Rust library is linked propertly.
--
-- Expect: "key length cannot be anything else than 32 characters"
test :: IO ()
test = do
 res <- putEdKey 
    (Id testKeyID) 
    (Label (encodeUtf8 $ pack "foo")) 
    (Domains 1)
    secretKey -- (encodeUtf8 $ pack $ take 32 $ repeat 'a')
    True
 let res' = assert (res == True) ("test passed " <> (show $ res == True))
 putStrLn("result " <> show res')
 putStrLn("res is " <> show res)
 