{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

main :: IO ()
main = do
  let passwordHash = sha1 "zaim9"
  putStrLn $ findPassword passwordHash
