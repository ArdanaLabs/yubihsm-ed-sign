module Api(app,SignApi) where

import Servant
  ((:<|>)((:<|>))
  ,(:>)
  ,Application
  ,Get
  ,Handler
  ,PlainText
  ,Post
  ,Proxy(Proxy)
  ,ReqBody
  ,Server
  ,serve
  )
import Control.Monad.IO.Class (MonadIO(liftIO))
import Lib (signWithEdKey, Id (Id),getPubKey)
import Hex(fromHex,toHex)

app :: Application
app = serve @SignApi Proxy server

type SignApi =
  "sign" :> ReqBody '[PlainText] String :> Post '[PlainText] String
  :<|> "pubkey" :> Get '[PlainText] String

server :: Server SignApi
server =
  signTx
  :<|> getPK

signTx :: String -> Handler String
signTx w = do
  let bytes = fromHex w
  liftIO $ print bytes
  signed <- liftIO $ signWithEdKey (Id 200) bytes True
  return $ toHex signed

getPK :: Handler String
getPK = do
  pk <- liftIO $ getPubKey (Id 200) True
  return $ toHex pk

