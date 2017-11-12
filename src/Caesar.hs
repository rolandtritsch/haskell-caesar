{-|
Module      : Caesar
Description : An implementation of the Caesar cipher
Copyright   : (c) Roland Tritsch, 2017
License     : GPL-3
Maintainer  : roland@tritsch.org
Stability   : experimental
Portability : POSIX

This is an implementation of the [Caesar cipher](https://en.wikipedia.org/wiki/Caesar_cipher).
-}
module Caesar (
  encrypt,
  decrypt,
  alphabet
  ) where

import Data.List
import Data.Maybe

-- |The alphabet (means all of the chars) we support for encryption.
-- |TODO: Use type Alphabet as the type (instead of String).
alphabet :: String
alphabet =
  ['a' .. 'z'] ++
  ['A' .. 'Z'] ++
  ['0' .. '9'] ++
  "!@#$%^&*()_+-={}|[]:;<>?,./~`\\\"\' "

-- |To lookup a character by its position in an alphabet.
-- |Note: The lookup uses a lazily evaluatated endless concatuation of alphabets to support arbitrarily large keys (i.e. keys that are larger than the alphabet). So that we do not have to mess around with modolo operations.
lookup'
  :: Int -- ^The position to lookup in the alphabet. p can be a bigger than the size of the alphabet.
  -> Char -- ^Return the character at poistion p.
lookup' p = a' !! p where
  a' = go alphabet where
    go a'' = a'' ++ go a''

-- |To lookup a character by its position in a reversed alphabet.
-- |Note: This is used for decryption. Instead of going backwards in the alphabet it is easier to go forward in a reversed alphabet.
lookupr':: Int -> Char
lookupr' n = a' !! n where
  a' = go (reverse alphabet) where
    go a'' = a'' ++ go a''

-- |Find all positions of a given text in a given alphabet (e.g. "abc" -> [0, 1, 2])
findPositions
  :: String -- ^The text to find the positions of.
  -> String -- ^The alphabet to lookup the positions in.
  -> [Int] -- ^Return the list of positions.
findPositions t a = mapMaybe (\c -> findIndex (\c' -> c' == c) a) t

-- |Calculate the new positions from the old positions and the given key.
calcPositions
  :: Int -- ^The key to use to calculate the new positions.
  -> [Int] -- ^The old positions.
  -> [Int] -- ^Return the new (encrypted or decrypted) positions.
calcPositions k ps = map (\p -> p + k) ps

-- |Lookup the chars in the new position.
codePositions
  :: (Int -> Char) -- ^The lookup function (luf) to use to lookup the chars (lookup' for encryption or lookupr' for decryption).
  -> [Int] -- ^The list of positions to lookup.
  -> String -- ^Return the resulting list of chars/string.
codePositions luf ps = map (\p -> luf p) ps

-- |Encrypt a given text with a given key.
encrypt
  :: String -- ^The text to encrypt. Note: We check that all chars in the text are in the alphabet. Otherwise we abort the encryption. TODO: Use PlainText as the type.
  -> Int -- ^The key to use to encrypt the text. Can be any Int. Pos and Neg. We take the abs of it, before we pass it on.
  -> String -- ^Return the encrypted text. TODO: Use EncryptedText as the type.
encrypt "" _ = ""
encrypt t k
  | all (\c -> elem c alphabet) t = encrypt' t (abs k) alphabet
  | otherwise = "Unknown char in text. Cannot encrypt."

-- |Encrypt a given text with a given key using the alphabet.
encrypt'
  :: String -- ^The text to encrypt.
  -> Int -- ^The key to use.
  -> String -- ^The alphabet to use.
  -> String -- ^Return the encrypted text.
encrypt' t k a = do
  let positions = findPositions t a
  let encryptedPositions = calcPositions k positions
  let encrypted = codePositions lookup' encryptedPositions
  encrypted

-- |Decrypt a given text with a given key.
decrypt
  :: String -- ^The text to encrypt.
  -> Int -- ^The key to use.
  -> String -- ^Return the encrypted text.
decrypt "" _ = ""
decrypt t k
  | all (\c -> elem c alphabet) t = decrypt' t (abs k) alphabet
  | otherwise = "Unknown char in text. Cannot decrypt."

-- |Decrypt a given text with a given key using the alphabet.
-- |Note: Decryption is implemented by moving forward on a reversed alphabet. That saves a lot of arkward modolo calculations.
decrypt'
  :: String -- ^The text to decrypt.
  -> Int -- ^The key to use.
  -> String -- ^The alphabet to use.
  -> String -- ^Return the decrypted text.
decrypt' t k a = do
  let positions = findPositions t (reverse a)
  let decryptedPositions = calcPositions k positions
  let decrypted = codePositions lookupr' decryptedPositions
  decrypted
