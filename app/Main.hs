{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust)
import Crypto.Hash
import Control.Parallel.Strategies

sha1 :: B.ByteString -> Digest SHA1
sha1 = hash

allStrings :: [String]
allStrings = [ c : s | s <- "" : allStrings, c <- ['a'..'z'] ++ ['0'..'9'] ]

checkPassword hash string
  | (sha1 $ B.pack string) == hash = Just string
  | otherwise = Nothing

getPassword :: Maybe String -> Maybe String -> Maybe String
getPassword Nothing maybePassword = maybePassword
getPassword maybePassword Nothing = maybePassword

main :: IO ()
main = do
  let passwordHash = sha1 $ B.pack "zaim9"
  -- runEval $ parBuffer 10000 rseq $
  let maybePasswords = map (checkPassword passwordHash) allStrings
  let findPassword = head $ filter isJust maybePasswords
  putStrLn $ show findPassword
