import Network.Wai.Handler.Warp
import System.IO
import Lib (secretKey,putEdKey,Id (Id), Label (Label),Domains(Domains) )
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

