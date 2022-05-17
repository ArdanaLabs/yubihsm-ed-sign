{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad.IO.Class ()
import qualified Data.ByteString as B
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Test.HUnit ( assertBool, assertEqual, runTestTT, Test(..) )
import Lib
    ( Domains(Domains),
      Label(Label),
      Id(Id),
      signature,
      secretKey,
      message,
      putEdKey,
      signWithEdKey )


testKeyID = 200;
main :: IO ()
main = do
    _ <- runTestTT tests
    return ()

tests :: Test
tests = 
    TestList [ TestLabel "test1" helloWorldTest
             , TestLabel "putEdKey Test" putEdKeyTest
             , TestLabel "signWithEdKey Test" signWithEdKeyTest]

helloWorldTest :: Test
helloWorldTest = TestCase $ do
    assertEqual "hello world" True True
    
    
putEdKeyTest :: Test
putEdKeyTest = TestCase $ do
    res <- putEdKey 
                (Id testKeyID) 
                (Label (encodeUtf8 $ pack "foo")) 
                (Domains 1)
                secretKey 
                True
    assertBool "putEdKey failed" res

signWithEdKeyTest :: Test
signWithEdKeyTest = TestCase $ do
    sig :: B.ByteString <- 
        signWithEdKey (Id testKeyID) message True
    assertEqual "signWithEdKeyTest failed" sig signature 