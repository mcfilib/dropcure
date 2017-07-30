module Main where

-- EXTERNAL

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.IO (BufferMode(..), hSetBuffering, stdout)

-- INTERNAL

import Consumer.Client
import Consumer.Server

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case listToMaybe args of
    Just "client" ->
      client
    _             ->
      server
