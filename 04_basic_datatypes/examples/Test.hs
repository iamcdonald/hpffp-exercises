module Main where

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "Chapter iv"
    [ exercisesSuite,
      correctingSyntaxSuite
    ]

exercisesSuite :: TestTree
exercisesSuite =
  testGroup
    "Exercises"
    [ testCase "2." two,
      testCase "4." four,
      testCase "5." five,
      testCase "7." seven,
      testCase "8." eight,
      testCase "9." nine,
      testCase "10." ten
    ]
  where
    awesome = ["Papuchon", "curry", ":)"]
    also = ["Quake", "The Simons"]
    allAwesome = [awesome, also]
    two :: Assertion
    two = do
      assertEqual "" 5 $ length [1, 2, 3, 4, 5]
      assertEqual "" 3 $ length [(1, 2), (2, 3), (3, 4)]
      assertEqual "" 2 $ length allAwesome
      assertEqual "" 5 $ length (concat allAwesome)
    four :: Assertion
    -- 6 / length [1, 2, 3] - `/` requires a Fractional Int as denominator where as length returns an Int
    -- `div` will allow this to work
    four = assertEqual "" 2 $ 6 `div` length [1, 2, 3]
    five :: Assertion
    five = assertEqual "" True $ 2 + 3 == 5
    seven :: Assertion
    seven = do
      -- will not work
      -- length [1, 'a', 3, 'b'] -- mixing types in list
      -- (8 == 8) && 9 -- literal '9' is not a bool so cannot be passed to `&&`
      assertEqual "" True $ length allAwesome == 2
      assertEqual "" 5 $ length allAwesome + length awesome
      assertEqual "" False $ (8 == 8) && ('b' < 'a')
    eight :: Assertion
    eight = do
      assertEqual "" True $ isPalindrome "civic"
      assertEqual "" False $ isPalindrome "radius"
      where
        isPalindrome :: (Eq a) => [a] -> Bool
        isPalindrome x = x == reverse x
    nine :: Assertion
    nine = do
      assertEqual "" 12 $ myAbs 12
      assertEqual "" 19 $ myAbs (-19)
      where
        myAbs :: Integer -> Integer
        myAbs x = if x < 0 then (-x) else x
    ten :: Assertion
    ten = do
      assertEqual "" ((2, 4), (1, 3)) $ f (1, 2) (3, 4)
      where
        f :: (a, b) -> (c, d) -> ((b, d), (a, c))
        f x y = ((snd x, snd y), (fst x, fst y))

correctingSyntaxSuite :: TestTree
correctingSyntaxSuite =
  testGroup
    "Correcting syntax"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three
    ]
  where
    one :: Assertion
    one = assertEqual "" 6 $ f "radar"
      where
        x = (+)
        f xs = w `x` 1
          where
            w = length xs
    two :: Assertion
    two = assertEqual "" 1 $ myId 1
      where
        myId = \x -> x
    three :: Assertion
    three = assertEqual "" 1 $ f (1, 2)
      where
        f (a, b) = a
