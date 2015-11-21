module Lib
    ( findPassword
    , sha1
    ) where

import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust)
import Crypto.Hash
import Control.Parallel.Strategies

sha1 :: String -> Digest SHA1
sha1 = hash . B.pack

allStrings :: [String]
allStrings = [ c : s | s <- "" : allStrings, c <- ['a'..'z'] ++ ['0'..'9'] ]

checkPassword :: Digest SHA1 -> String -> Maybe String
checkPassword hash string
  | (sha1 string) == hash = Just string
  | otherwise = Nothing

getPassword :: Maybe String -> Maybe String -> Maybe String
getPassword Nothing maybePassword = maybePassword
getPassword maybePassword Nothing = maybePassword

-- runEval $ parBuffer 10000 rseq $
findPassword :: Digest SHA1 -> String
findPassword passwordHash = show $ head $ filter isJust $ map (checkPassword passwordHash) allStrings
