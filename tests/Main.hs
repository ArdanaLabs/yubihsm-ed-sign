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
      signWithEdKey, getPubKey, publicKey )
import Data.Word (Word16)
import System.Exit (die)
import Control.Monad (void)

testKeyID :: Word16
testKeyID = 200;

main :: IO ()
main = do
    void $ die "test failure"
    _ <- runTestTT tests
    return ()

tests :: Test
tests =
    TestList [ TestLabel "test1" helloWorldTest
             , TestLabel "putEdKey Test" putEdKeyTest
             , TestLabel "signWithEdKey Test" signWithEdKeyTest
             , TestLabel "getPublicKey" getPublicKeyTest]

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

getPublicKeyTest :: Test
getPublicKeyTest = TestCase $ do
    res <- getPubKey (Id testKeyID) True
    assertEqual "getPublicKeyTest failed" res publicKey
