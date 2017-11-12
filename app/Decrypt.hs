{-|
Module      : Main - decrypt
Description : Main module to implement decrypt command line binary.
Copyright   : (c) Roland Tritsch, 2017
License     : GPL-3
Maintainer  : roland@tritsch.org
Stability   : experimental
Portability : POSIX

Implement binary to do decryption.

Usage: decrypt <text> <key>
-}
module Main where

import System.Environment (getArgs)
import Control.Exception.Base

import Caesar (decrypt)

-- |Do the command line processing and call decrypt.
main :: IO ()
main = do
  args <- getArgs
  let text = assert ((length args) == 2) (args !! 0)
  let key = read (args !! 1) :: Int

  putStrLn (decrypt text key)
