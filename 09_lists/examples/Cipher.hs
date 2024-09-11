module Cipher where

import Data.Char
import Data.List
import Data.Maybe

ceaser :: Int -> String -> String
ceaser s (c : cs) = transform c : ceaser s cs
  where
    alpha = ['a' .. 'z']
    swap char
      | isJust index = alpha !! rem (fromJust index + s) (length alpha)
      | otherwise = char
      where
        index = elemIndex char alpha
    transform char
      | isUpper char = (toUpper . swap . toLower) char
      | otherwise = swap char
ceaser _ _ = []

unCeaser :: Int -> String -> String
unCeaser s = ceaser (-s)
