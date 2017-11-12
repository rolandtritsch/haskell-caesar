module Main where

import System.Environment (getArgs)
import Control.Exception.Base

import Caesar (decrypt)

main :: IO ()
main = do
  args <- getArgs
  let text = assert ((length args) == 2) (args !! 0)
  let key = read (args !! 1) :: Int

  putStrLn (decrypt text key)
