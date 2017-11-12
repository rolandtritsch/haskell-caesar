module Main where

import System.Environment (getArgs)
import Control.Exception.Base

import Caesar (encrypt)

main :: IO ()
main = do
  args <- getArgs
  let text = assert ((length args) == 2) (args !! 0)
  let key = read (args !! 1) :: Int

  putStrLn (encrypt text key)
