module Main where

import Arith4
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain suite

data Dummy = Dummy deriving (Eq)

suite :: TestTree
suite =
  testGroup
    "Chapter 7 - More Functional Patterns"
    [ varietyPackSuite,
      casePracticeSuite,
      letsWriteCodeSuite
    ]

varietyPackSuite :: TestTree
varietyPackSuite =
  testGroup
    "Variety Pack"
    [ testCase "2." two
    ]
  where
    two :: Assertion
    two =
      assertEqual "" ((1, 4), (3, 6)) $ fn (1, 2, 3) (4, 5, 6)
      where
        fn :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
        fn (a, _, c) (d, _, f) = ((a, d), (c, f))

casePracticeSuite :: TestTree
casePracticeSuite =
  testGroup
    "Case Practice"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three
    ]
  where
    one :: Assertion
    one = do
      assertEqual "" (functionC 1 3) (caseFunctionC 1 3)
      assertEqual "" (functionC 3 1) (caseFunctionC 3 1)
      where
        functionC x y = if (x > y) then x else y
        caseFunctionC x y = case (x > y) of
          True -> x
          False -> y
    two :: Assertion
    two = do
      assertEqual "" (ifEvenAdd2 2) (caseIfEvenAdd2 2)
      assertEqual "" (ifEvenAdd2 5) (caseIfEvenAdd2 5)
      where
        ifEvenAdd2 n = if even n then (n + 2) else n
        caseIfEvenAdd2 n = case even n of
          True -> n + 2
          False -> n
    three :: Assertion
    three = do
      assertEqual "" (-1) $ nums (-2)
      assertEqual "" 1 $ nums 2
      assertEqual "" 0 $ nums 0
      where
        nums x =
          case compare x 0 of
            LT -> -1
            GT -> 1
            EQ -> 0

guardDutySuite :: TestTree
guardDutySuite =
  testGroup
    "Guard Duty"
    [ testCase "3-5." threeFourFive,
      testCase "6-8." sixSevenEight
    ]
  where
    threeFourFive :: Assertion
    threeFourFive = do
      assertEqual "" True $ pal "radar"
      assertEqual "" True $ pal [1, 2, 3, 2, 1]
      assertEqual "" False $ pal [1, 2, 3, 2, 2]
      where
        -- True when xs is a palindrome.
        pal :: (Eq a) => [a] -> Bool
        pal xs
          | xs == reverse xs = True
          | otherwise = False
    sixSevenEight :: Assertion
    sixSevenEight = do
      assertEqual "" (-1) $ numbers (-20)
      assertEqual "" 1 $ numbers 20
      assertEqual "" 0 $ numbers 0
      where
        -- An indication of whether its argument is a positive or negative number or 0.
        numbers :: (Ord a, Integral a) => a -> Integer
        numbers x
          | x < 0 = -1
          | x == 0 = 0
          | x > 0 = 1

letsWriteCodeSuite :: TestTree
letsWriteCodeSuite =
  testGroup
    "Lets write code"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three,
      testCase "5." five,
      testCase "6." six
    ]
  where
    one :: Assertion
    one = do
      assertEqual "" (tensDigit 20) (tensDigitDivMod 20)
      assertEqual "" (tensDigit 47) (tensDigitDivMod 47)
      assertEqual "" 4 (hunsD 496)
      where
        tensDigit :: (Integral a) => a -> a
        tensDigit x = d
          where
            xLast = x `div` 10
            d = xLast `mod` 10
        tensDigitDivMod :: (Integral a) => a -> a
        tensDigitDivMod = fst . flip divMod 10
        hunsD :: (Integral a) => a -> a
        hunsD = fst . flip divMod 100
    two :: Assertion
    two = do
      assertEqual "" 1 $ foldBoolCase 1 2 True
      assertEqual "" 2 $ foldBoolCase 1 2 False
      assertEqual "" 1 $ foldBoolGuard 1 2 True
      assertEqual "" 2 $ foldBoolGuard 1 2 False
      where
        foldBoolCase :: (Ord a) => a -> a -> Bool -> a
        foldBoolCase x y b = case b of
          True -> x
          False -> y
        foldBoolGuard :: (Ord a) => a -> a -> Bool -> a
        foldBoolGuard x y b
          | b = x
          | not b = y
    three :: Assertion
    three = do
      assertEqual "" (3, 12) $ g round (3.4, 12)
      where
        g :: (a -> b) -> (a, c) -> (b, c)
        g f (a, c) = (f a, c)
    five :: Assertion
    five = do
      assertEqual "" 2 $ roundTrip 2
      assertEqual "" 2 $ roundTripPF 2
    six :: Assertion
    six = assertEqual "" 4 $ roundTrip2 (4 :: Integer)
