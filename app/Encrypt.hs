{-|
Module      : Main - encrypt
Description : Main module to implement encrypt command line binary.
Copyright   : (c) Roland Tritsch, 2017
License     : GPL-3
Maintainer  : roland@tritsch.org
Stability   : experimental
Portability : POSIX

Implement binary to do encryption.

Usage: encrypt <text> <key>
-}
module Main where

import System.Environment (getArgs)
import Control.Exception.Base

import Caesar (encrypt)

-- |Do the command line processing and call decrypt.
main :: IO ()
main = do
  args <- getArgs
  let text = assert ((length args) == 2) (args !! 0)
  let key = read (args !! 1) :: Int

  putStrLn (encrypt text key)
