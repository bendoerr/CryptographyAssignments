module Assignment1.Bits where

import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Word       (Word8)

numberOfSetBits :: Word8 -> Int
numberOfSetBits x
    | x == 0    = 0
    | otherwise = 1 + (numberOfSetBits (x .&. (x - 1)))

hammingDistance :: ByteString -> ByteString -> Int
hammingDistance a b
    | (B.length a) == (B.length b) = sum (map (\ (x, y) -> numberOfSetBits (xor x y)) (B.zip a b))
    | otherwise                    = error "Lengths of both strings must be equal."

avgHammingDistance bss = (toRational $ fst $ foldl sumHam (0, tail bss) bss) / (toRational $ (length bss) - 1)
    where sumHam (acc, []) _ = (acc, [])
          sumHam (acc, rem) bs = (acc + ((toRational $hammingDistance bs $ head rem) / (toRational $ B.length bs)), tail rem)
