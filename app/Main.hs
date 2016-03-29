{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Assignment1.XOR
import Assignment1.Convert

import System.Environment (getArgs)
import Data.List (intercalate)
import Control.Monad
import Text.Printf (printf)
import Data.Text.Encoding (decodeLatin1)
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Char8 as BSC
import Data.Char (chr)

main :: IO ()
main = do args <- getArgs
          case (args) of
            ("assignment1":_) -> assignment1
            ("challenge1":_) -> challenge1

assignment1 = do
        ciphertext <- (fmap (fromHex) B.getContents)
        putStr "\n - Given Ciphertext:     "
        TIO.putStrLn $ decodeLatin1 $ toHex ciphertext
        let topKeys = take 1 $ guessKeySize 1 13 ciphertext in
            mapM_ (\tk -> assignment1_work (fst $ tk) ciphertext) topKeys

assignment1_work kLen ciphertext = do
        putStrLn $ printf "\n - Guessed key length:     %d" kLen
        putStr            " - Evaluated key as:       "
        TIO.putStrLn $ decodeLatin1 $ toHex key
        putStr            " - Evaluated plaintext as: "
        TIO.putStrLn $ decodeLatin1 plaintext
    where (key, plaintext) = recoverXorRepeat kLen ciphertext

challenge1 = do
        ciphertext <- (fmap (fromBase64 . B.filter (not . (==) 0x0A)) B.getContents)
        putStr "\n - Given Ciphertext:     "
        TIO.putStrLn $ decodeLatin1 $ toBase64 ciphertext
        let topKeys = take 2 $ guessKeySize 1 40 ciphertext in
            mapM_ (\tk -> challenge1_work (fst $ tk) ciphertext) topKeys

challenge1_work kLen ciphertext = do
        putStrLn $ printf "\n - Guessed key length:     %d" kLen
        putStr            " - Evaluated key as:       "
        BSC.putStrLn $ key
        putStr            " - Evaluated plaintext as: "
        BSC.putStrLn $ plaintext
    where (key, plaintext) = recoverXorRepeat kLen ciphertext
