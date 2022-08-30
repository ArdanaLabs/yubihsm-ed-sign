{-# LANGUAGE DataKinds #-}

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Control.Monad.IO.Class (MonadIO(liftIO))
import Lib (secretKey,putEdKey,signWithEdKey, Id (Id), Label (Label),Domains(Domains), getPubKey)
import Data.String (IsString(fromString))
import App(app)

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

