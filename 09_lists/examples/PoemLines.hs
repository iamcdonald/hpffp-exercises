module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen =
  "Could frame thy fearful\
  \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines [] = []
myLines s = takeWhile (not . isNewLine) s : myLines ((dropWhile isNewLine . dropWhile (not . isNewLine)) s)
  where
    isNewLine = (==) '\n'
