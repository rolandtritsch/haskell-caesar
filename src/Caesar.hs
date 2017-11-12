module Caesar (
  encrypt,
  decrypt,
  alphabet
  ) where

import Data.List
import Data.Maybe
import Control.Exception.Base

alphabet :: String
alphabet =
  ['a' .. 'z'] ++
  ['A' .. 'Z'] ++
  ['0' .. '9'] ++
  [' ', '!', '?', '.', ';', ':', '(', ')', '"']

lookup' :: Int -> Char
lookup' n = assert(n >= 0) a' !! n where
  a' = go alphabet where
    go a'' = a'' ++ go a''

lookupr' :: Int -> Char
lookupr' n = assert(n >= 0) a' !! n where
  a' = go (reverse alphabet) where
    go a'' = a'' ++ go a''

findPositions :: String -> String -> [Int]
findPositions t a = mapMaybe (\c -> findIndex (\c' -> c' == c) a) t

calcPositions :: Int -> [Int] -> [Int]
calcPositions k ps = map (\p -> p + k) ps

codePositions :: (Int -> Char) -> [Int] -> String
codePositions luf ps = map (\p -> luf p) ps

encrypt :: String -> Int -> String
encrypt "" _ = ""
encrypt t k = encrypt' t (abs k) alphabet

encrypt' :: String -> Int -> String -> String
encrypt' t k a = do
  let positions = findPositions t a
  let encryptedPositions = calcPositions k positions
  let encrypted = codePositions lookup' encryptedPositions
  encrypted

decrypt :: String -> Int -> String
decrypt "" _ = ""
decrypt t k = decrypt' t (abs k) alphabet

decrypt' :: String -> Int -> String -> String
decrypt' t k a = do
  let positions = findPositions t (reverse a)
  let decryptedPositions = calcPositions k positions
  let decrypted = codePositions lookupr' decryptedPositions
  decrypted
