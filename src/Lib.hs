module Lib
    ( findPassword
    , sha1
    ) where

import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust)
import Crypto.Hash
import qualified Control.Parallel.Strategies as S
import Control.Parallel.Strategies (NFData, ($||))

sha1 :: String -> Digest SHA1
sha1 = hash . B.pack

allStrings :: [String]
allStrings = [ c : s | s <- "" : allStrings, c <- ['a'..'z'] ++ ['0'..'9'] ]

parFilter :: (NFData a) => (a -> Bool) -> [a] -> [a]
parFilter p = S.withStrategy (S.parBuffer 100 S.rseq) . filter p

checkPassword :: Digest SHA1 -> String -> Bool
checkPassword hash string = (sha1 string) == hash

findPassword :: Digest SHA1 -> String
findPassword passwordHash = (head . filter checkThePassword) allStrings
  where checkThePassword = checkPassword passwordHash
