module Main where

import Test.Tasty
import Test.Tasty.HUnit
import WordNumber

main :: IO ()
main = defaultMain suite

data Dummy = Dummy deriving (Eq)

suite :: TestTree
suite =
  testGroup
    "Chapter 8 - Recursion"
    [ typesSuite,
      curryingSuite,
      recursionSuite,
      fixingDividedBySuite,
      mccarthy91Suite,
      wordNumberSuite
    ]

typesSuite :: TestTree
typesSuite =
  testGroup
    "Types"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3-4." threeFour
    ]
  where
    one :: Assertion
    one =
      assertEqual "" True $ all (all (\b -> b || not b)) x
      where
        -- wouldn't compile if this type signature was not correct
        x :: [[Bool]]
        x = [[True, False], [True, True], [False, True]]
    two :: Assertion
    two =
      assertEqual "" True $ all (all (\b -> b || not b)) x
      where
        x :: [[Bool]]
        -- b
        x = [[3 == 3], [6 > 5], [3 < 4]]
    threeFour :: Assertion
    threeFour =
      assertEqual "" "HelloWorld" $ func "Hello" "World"
      where
        func :: [a] -> [a] -> [a]
        func x y = x ++ y

curryingSuite :: TestTree
curryingSuite =
  testGroup
    "Currying"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three,
      testCase "4." four,
      testCase "5." five,
      testCase "6." six
    ]
  where
    cattyConny :: String -> String -> String
    cattyConny x y = x ++ " mrow " ++ y
    -- fill in the types
    flippy :: String -> String -> String
    flippy = flip cattyConny
    appedCatty :: String -> String
    appedCatty = cattyConny "woops"
    frappe :: String -> String
    frappe = flippy "haha"

    one :: Assertion
    one = assertEqual "" "woops mrow woohoo!" $ appedCatty "woohoo!"
    two :: Assertion
    two = assertEqual "" "1 mrow haha" $ frappe "1"
    three :: Assertion
    three = assertEqual "" "woops mrow 2 mrow haha" $ frappe (appedCatty "2")
    four :: Assertion
    four = assertEqual "" "woops mrow blue mrow haha" $ appedCatty (frappe "blue")
    five :: Assertion
    five = assertEqual "" "pink mrow haha mrow green mrow woops mrow blue" $ cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
    six :: Assertion
    six = assertEqual "" "are mrow Pugs mrow awesome" $ cattyConny (flippy "Pugs" "are") "awesome"

recursionSuite :: TestTree
recursionSuite =
  testGroup
    "Recursion"
    [ testCase "2." two,
      testCase "3." three
    ]
  where
    two :: Assertion
    two = do
      assertEqual "" 3 $ add 2
      assertEqual "" 55 $ add 10
      assertEqual "" 15 $ add 5
      where
        add :: (Integral a) => a -> a
        add = go 0
          where
            go :: (Integral a) => a -> a -> a
            go s 0 = s
            go s n = go (s + n) (n - 1)
    three :: Assertion
    three = do
      assertEqual "" 20 $ mult 10 2
      assertEqual "" 55 $ mult 5 11
      assertEqual "" 12 $ mult 3 4
      where
        mult :: (Integral a) => a -> a -> a
        mult a b = go a b
          where
            go s 1 = s
            go s mt = go (s + a) (mt - 1)

data DividedResult = Result Integer | DividedByZero deriving (Show, Eq)

fixingDividedBySuite :: TestTree
fixingDividedBySuite =
  testGroup
    "Fixing dividedBy"
    [ testCase "." one
    ]
  where
    one :: Assertion
    one = do
      assertEqual "" (Result 5) $ dividedBy (25 :: Integer) 5
      assertEqual "" (Result (-5)) $ dividedBy (10 :: Integer) (-2)
      assertEqual "" (Result 5) $ dividedBy (-10 :: Integer) (-2)
      assertEqual "" DividedByZero $ dividedBy (-10 :: Integer) 0
      where
        dividedBy :: (Integral a) => a -> a -> DividedResult
        dividedBy _ 0 = DividedByZero
        dividedBy num denom
          | neg = Result $ negate v
          | otherwise = Result v
          where
            neg = num * denom < 0
            go n d count
              | n < d = count
              | otherwise = go (n - d) d (count + 1)
            v = go (abs num) (abs denom) 0

mccarthy91Suite :: TestTree
mccarthy91Suite =
  testGroup
    "McCarthy 91"
    [ testCase "." one
    ]
  where
    one :: Assertion
    one =
      assertEqual "" expected $ map mc91 [95 .. 110]
      where
        expected = [91, 91, 91, 91, 91, 91, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100]
        mc91 x
          | x > 100 = x - 10
          | otherwise = 91

wordNumberSuite :: TestTree
wordNumberSuite =
  testGroup
    "Word Number"
    [ testCase "." one
    ]
  where
    one :: Assertion
    one = assertEqual "" "one-two-three-two-four-five-four-six" $ digitToWord 12324546
