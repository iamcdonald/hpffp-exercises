module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = concat $ intersperse "-" $ map wordNumber $ digits n

digits :: Int -> [Int]
digits n = map (\x -> read [x] :: Int) $ show n

wordNumber :: Int -> String
-- deliberately left as partial pattern match as only covering 1-9
wordNumber 0 = "zero"
wordNumber 1 = "one"
wordNumber 2 = "two"
wordNumber 3 = "three"
wordNumber 4 = "four"
wordNumber 5 = "five"
wordNumber 6 = "six"
wordNumber 7 = "seven"
wordNumber 8 = "eight"
wordNumber 9 = "nine"
