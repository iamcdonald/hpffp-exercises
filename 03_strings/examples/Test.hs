module Main where

import BuildingFunctions
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "Chapter iii"
    [ readingSyntaxSuite,
      buildingFunctionsSuite
    ]

readingSyntaxSuite :: TestTree
readingSyntaxSuite =
  testGroup
    "Reading syntax"
    [ testCase "1." one,
      testCase "2." two
    ]
  where
    one :: Assertion
    one = do
      -- concat [[1, 2, 3], [4, 5, 6]] - fine
      assertEqual "" [1, 2, 3, 4, 5, 6] $ concat [[1, 2, 3], [4, 5, 6]]
      -- ++ [1, 2, 3] [4, 5, 6] -- requires change
      assertEqual "" [1, 2, 3, 4, 5, 6] $ [1, 2, 3] ++ [4, 5, 6]
      -- (++) "hello" " world" - fine
      assertEqual "" "hello world" $ (++) "hello" " world"
      -- ["hello" ++ " world] - fine
      assertEqual "" ["hello world"] ["hello" ++ " world"]
      -- 4 !! "hello" - requires change
      assertEqual "" (head "o") $ "hello" !! 4
      -- (!!) "hello" 4 - fine
      assertEqual "" (head "o") $ (!!) "hello" 4
      -- take "4 lovely" - requires change
      assertEqual "" "love" $ take 4 "lovely"
      -- take 3 "awesome" - fine
      assertEqual "" "awe" $ take 3 "awesome"
    two :: Assertion
    two = do
      assertEqual "" code1 answer4
      assertEqual "" code2 answer3
      assertEqual "" code3 answer5
      assertEqual "" code4 answer1
      assertEqual "" code5 answer2
      where
        code1 = concat [[1 * 6], [2 * 6], [3 * 6]]
        code2 = "rain" ++ drop 2 "elbow"
        code3 = 10 * head [1, 2, 3]
        code4 = (take 3 "Julie") ++ (tail "yes")
        code5 = concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]]
        answer1 = "Jules"
        answer2 = [2, 3, 5, 6, 8, 9]
        answer3 = "rainbow"
        answer4 = [6, 12, 18]
        answer5 = 10

buildingFunctionsSuite :: TestTree
buildingFunctionsSuite =
  testGroup
    "Building Functions"
    [ testCase "1.a" a,
      testCase "1.b" b,
      testCase "1.c" c,
      testCase "2." two,
      testCase "3." three,
      testCase "4." four,
      testCase "5." five
    ]
  where
    a :: Assertion
    a = assertEqual "" "Curry is awesome!" $ f "Curry is awesome"
      where
        f x = x ++ "!"
    b :: Assertion
    b = assertEqual "" 'y' $ f "Curry is awesome!"
      where
        f x = x !! 4
    c :: Assertion
    c = assertEqual "" "awesome!" $ f "Curry is awesome!"
      where
        f = drop 9
    two :: Assertion
    two = do
      assertEqual "" "Curry is awesome!" $ addExclamation "Curry is awesome"
      assertEqual "" 'y' $ getLastLetterOfFirstWord "Curry is awesome!"
      assertEqual "" "awesome!" $ getLastWord "Curry is awesome!"
    three :: Assertion
    three = assertEqual "" 'r' $ thirdLetter "Curry is awesome"
    four :: Assertion
    four = do
      assertEqual "" 'r' $ letterIndex 3
      assertEqual "" 'w' $ letterIndex 10
    five :: Assertion
    five = assertEqual "" "awesome is Curry" $ rvrs "Curry is awesome"
