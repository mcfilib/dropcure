module Main where

import System.IO (BufferMode(..), hSetBuffering, stdout)

-- EXTERNAL

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)

-- INTERNAL

import Producer.Client
import Producer.Config
import Producer.Server
import Producer.Types

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case listToMaybe args of
    Just "client" ->
      client
    _             ->
      server
