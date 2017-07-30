module Main where

-- EXTERNAL

import System.IO (BufferMode(..), hSetBuffering, stdout)

-- INTERNAL

import Consumer.Server

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  server
