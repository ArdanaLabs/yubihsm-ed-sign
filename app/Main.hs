{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import Control.Monad.IO.Class (MonadIO(liftIO))
import Lib (secretKey,putEdKey,signWithEdKey, Id (Id), Label (Label),Domains(Domains))
import Data.String (IsString(fromString))

type SignApi =
  "sign" :> ReqBody '[JSON, PlainText] String :> Post '[JSON] Tx

main :: IO ()
main = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (do
          hPutStrLn stderr ("listening on port " ++ show port)
          print =<< putEdKey (Id 200) (Label $ fromString "testkey") (Domains 1) secretKey True
                          )
        defaultSettings
  runSettings settings mkApp

mkApp :: Application
mkApp = serve @SignApi Proxy server

server :: Server SignApi
server = signTx

signTx :: String -> Handler Tx
signTx w = do
  let bytes = fromString w
  liftIO $ print bytes
  signed <- liftIO $ signWithEdKey (Id 200) bytes True
  return $ Tx $ show signed

newtype Tx
  = Tx {
    value :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Tx
instance FromJSON Tx

{-
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Servant.API
import Servant



type Sign = "sign" :> Post '[String] String

main :: IO ()
main = run 555 (serve @Sign Proxy app)

app :: Server Sign
app = undefined
-}
