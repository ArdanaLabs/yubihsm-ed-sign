{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Lib (putEdKey, signWithEdKey, Id(..), Label(..), Domains(..))

main :: IO ()
main = do
  let id = Id 100
      label = Label "testkey1"
      domains = Domains 0x0001
  putStrLn "Enter the key as a list of byte values:"
  key <- fmap (B.pack . read) getLine
  -- let key = B.pack keyL
  putStrLn "Trying to put the key on YubiHSM"
  putEdKey id label domains key
  putStrLn "Wrote the key successfully"
  putStrLn "Enter the message to sign as a list of byte values:"
  msg <- fmap (B.pack . read) getLine
  putStrLn "Signing the message"
  sig <- signWithEdKey id msg
  putStrLn "The signature (as a list of byte values) is:"
  print (B.unpack sig)
