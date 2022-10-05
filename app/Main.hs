{-# LANGUAGE LambdaCase #-}
import System.Environment (getArgs)
import Api(signTx,getPK)
import System.Exit (die)

main :: IO ()
main =
  getArgs >>= \case
    ["getPubKey"] -> getPK >>= putStrLn
    ["sign",tx] -> signTx tx >>= putStrLn
    _ -> die "expected `getpubkey` or `sign <some_tx>`"
