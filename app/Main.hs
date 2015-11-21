{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

main :: IO ()
main = do
  putStrLn "Enter the clear password to hack"
  password <- getLine
  let passwordHash = sha1 password
  putStrLn $ "Start computing hashes to find " ++ (show passwordHash)
  putStrLn $ findPassword passwordHash
