module Lib
    ( findPassword
    , sha1
    ) where

import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust)
import Crypto.Hash
import qualified Control.Parallel.Strategies as S

sha1 :: String -> Digest SHA1
sha1 = hash . B.pack

allStrings :: [String]
allStrings = [ c : s | s <- "" : allStrings, c <- ['a'..'z'] ++ ['0'..'9'] ]

parAllHashes :: [(String, Digest SHA1)]
parAllHashes = S.runEval $ S.parBuffer 16 S.rseq $ map (\x -> (x, sha1 x)) allStrings

allHashes :: [(String, Digest SHA1)]
allHashes = map (\x -> (x, sha1 x)) allStrings

checkPassword :: Digest SHA1 -> String -> Bool
checkPassword hash string = (sha1 string) == hash

findPassword :: Digest SHA1 -> String
findPassword passwordHash = (fst . head) (filter (testTuple passwordHash) parAllHashes)

testTuple :: Digest SHA1 -> (String, Digest SHA1) -> Bool
testTuple a (_, b) = a == b
