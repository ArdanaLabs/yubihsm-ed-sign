{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Control.Monad.IO.Class (MonadIO(liftIO))
import Lib (secretKey,putEdKey,signWithEdKey, Id (Id), Label (Label),Domains(Domains), getPubKey)
import Data.String (IsString(fromString))

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
  runSettings settings app

app :: Application
app = serve @SignApi Proxy server

type SignApi =
  "sign" :> ReqBody '[JSON, PlainText] String :> Post '[JSON] Payload
  :<|> "pubkey" :> Get '[JSON] Payload

server :: Server SignApi
server =
  signTx
  :<|> getPK

signTx :: String -> Handler Payload
signTx w = do
  let bytes = fromString w
  liftIO $ print bytes
  signed <- liftIO $ signWithEdKey (Id 200) bytes True
  return $ Payload $ show signed

getPK :: Handler Payload
getPK = do
  pk <- liftIO $ getPubKey (Id 200) True
  return $ Payload $ show pk

newtype Payload = Payload {value :: String}
  deriving (Eq, Show, Generic)

instance ToJSON Payload
instance FromJSON Payload
