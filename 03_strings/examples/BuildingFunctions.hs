module BuildingFunctions where

addExclamation :: String -> String
addExclamation x = x ++ "!"

-- maybe gone to far with this one and it should just be `x !! 4`
getLastLetterOfFirstWord :: String -> Char
getLastLetterOfFirstWord x = last $ head $ words x

getLastWord :: String -> String
getLastWord x = last $ words x

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String -> String
rvrs x = drop 9 x ++ drop 5 (take 9 x) ++ take 5 x
