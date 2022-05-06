import Lib
import Data.Text.Encoding
import Data.Text (pack)

main :: IO ()
main = test

-- | Use this to test if the Rust library is linked propertly.
--
-- Expect: "key length cannot be anything else than 32 characters"
test :: IO ()
test = 
  putEdKey 
    (Id 1) 
    (Label (encodeUtf8 $ pack "foo")) 
    (Domains 1)
    (encodeUtf8 $ pack $ take 32 $ repeat 'a')
