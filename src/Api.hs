module Api(app,SignApi) where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Servant
import Control.Monad.IO.Class (MonadIO(liftIO))
import Lib (signWithEdKey, Id (Id),getPubKey)
import Hex(fromHex,toHex)

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
  let bytes = fromHex w
  liftIO $ print bytes
  signed <- liftIO $ signWithEdKey (Id 200) bytes True
  return $ Payload $ toHex signed

getPK :: Handler Payload
getPK = do
  pk <- liftIO $ getPubKey (Id 200) True
  return $ Payload $ toHex pk

newtype Payload = Payload {value :: String}
  deriving stock (Eq, Show, Generic)

instance ToJSON Payload
instance FromJSON Payload
