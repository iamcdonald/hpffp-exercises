module Phone where

import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

data Button = Button Digit [Char]

data Phone = Phone [Button]

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have you ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

phone :: Phone
phone =
  Phone
    [ Button '1' "1",
      Button '2' "abc2",
      Button '3' "def3",
      Button '4' "ghi4",
      Button '5' "jkl5",
      Button '6' "mno6",
      Button '7' "pqrs7",
      Button '8' "tuv8",
      Button '9' "wxyz9",
      Button '*' "^",
      Button '0' "+ 0",
      Button '#' ".,"
    ]

reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps (Phone buttons) c
  | isUpper c = ('*', 1) : getCharPress (toLower c)
  | otherwise = getCharPress c
  where
    includes :: Char -> Button -> Bool
    includes c (Button _ cs) = c `elem` cs
    getCharPress :: Char -> [(Digit, Presses)]
    getCharPress char =
      [(d, maybe (error "CharNotFound") (+ 1) $ char `elemIndex` cs)]
      where
        (Button d cs) = fromMaybe (error "ButtonNotFound") (find (includes char) buttons)

cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead p = concatMap (reverseTaps p)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, p) culm -> p + culm) 0

mostPopularLetter :: String -> (Int, Char)
mostPopularLetter = maximumBy (comparing fst) . map (\c -> (length c, head c)) . group . sortBy (comparing Down)

mostPopularLetterCost :: String -> (Char, Int)
mostPopularLetterCost s = (char, count * fingerTaps (reverseTaps phone char))
  where
    (count, char) = mostPopularLetter s

coolestLtr :: [String] -> Char
coolestLtr = snd . maximumBy (comparing fst) . map (mostPopularLetter . filter (/= ' '))

coolestWord :: [String] -> String
coolestWord = head . maximumBy (compare `on` length) . group . sortBy (comparing Down) . concatMap words
