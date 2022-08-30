module Hex(toHex,fromHex) where

import Data.ByteString(ByteString,unpack,pack)
import Data.Word(Word8)
import Data.List.Split(chunksOf)
import Numeric (showHex)

toHex :: ByteString -> String
toHex = concatMap byteToHex . unpack

fromHex :: String -> ByteString
fromHex = pack . map (read . ("0x"<>)) . chunksOf 2

byteToHex :: Word8 -> String
byteToHex b = padToLen 2 '0' (showHex b "")

padToLen :: Int -> Char -> String -> String
padToLen len c w = replicate (len - length w) c <> w
