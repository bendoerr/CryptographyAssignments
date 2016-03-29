module Assignment1.Convert ( fromHex
                           , fromBase64
                           , fromDec
                           , toHex
                           , toBase64
                           , toDec ) where

import           Data.ByteString        (ByteString, pack, unpack)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import           Data.Word              (Word8)

fromHex :: ByteString -> ByteString
fromHex = fst . Base16.decode

toHex :: ByteString -> ByteString
toHex = Base16.encode

fromBase64 :: ByteString -> ByteString
fromBase64 = Base64.decodeLenient

toBase64 :: ByteString -> ByteString
toBase64 = Base64.encode

toDec :: ByteString -> [Word8]
toDec = unpack

fromDec :: [Word8] -> ByteString
fromDec = pack
