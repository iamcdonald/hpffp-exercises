module Main where

import FunWithFunctions
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "Chapter ii"
    [ parenthesizationSuite,
      equivalentExpressionsSuite,
      funWithFunctionsSuite
    ]

parenthesizationSuite :: TestTree
parenthesizationSuite =
  testGroup
    "Parenthesization"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three
    ]
  where
    one :: Assertion
    one = assertEqual "" given withParenthesis
      where
        given = 2 + 2 * 3 - 1
        withParenthesis = 2 + (2 * 3) - 1
    two :: Assertion
    two = assertEqual "" given withParenthesis
      where
        given = (^) 10 $ 1 + 1
        withParenthesis = 10 ^ (1 + 1)
    three :: Assertion
    three = assertEqual "" given withParenthesis
      where
        given = 2 ^ 2 * 4 ^ 5 + 1
        withParenthesis = ((2 ^ 2) * (4 ^ 5)) + 1

equivalentExpressionsSuite :: TestTree
equivalentExpressionsSuite =
  testGroup
    "Equivalent expressions"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three,
      testCase "4." four,
      testCase "5." five
    ]
  where
    one :: Assertion
    one = assertEqual "" True $ given == equivalent
      where
        given = 1 + 1
        equivalent = 2
    two :: Assertion
    two = assertEqual "" True $ given == equivalent
      where
        given = 10 ^ 2
        equivalent = 10 + 9 * 10
    three :: Assertion
    three = assertEqual "" False $ given == equivalent
      where
        given = 400 - 37
        equivalent = (-) 37 400
    four :: Assertion
    -- '/' does float division whereas 'div' sticks to Int (hence the cast to check equality)
    four = assertEqual "" False $ fromIntegral given == equivalent
      where
        given = 100 `div` 3
        equivalent = 100 / 3
    five :: Assertion
    five = assertEqual "" False $ given == equivalent
      where
        given = 2 * 5 + 18
        equivalent = 2 * (5 + 18)

funWithFunctionsSuite :: TestTree
funWithFunctionsSuite =
  testGroup
    "Fun with Functions"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3-4." three_four,
      testCase "5." five,
      testCase "7." seven
    ]
  where
    one :: Assertion
    one = do
      assertEqual "" 1135 $ 10 + waxOn
      assertEqual "" 1135 $ (+ 10) waxOn
      assertEqual "" (-1110) $ (-) 15 waxOn
      assertEqual "" 1110 $ (-) waxOn 15
    two :: Assertion
    two = assertEqual "" 3375 $ triplew waxOn
      where
        triplew _x = _x * 3
    three_four :: Assertion
    three_four = assertEqual "" waxOn waxOnF
    five :: Assertion
    five = assertEqual "" (triple waxOnF) $ triplew waxOn
      where
        triplew _x = _x * 3
    seven :: Assertion
    seven = do
      assertEqual "" 30 $ waxOff 10
      assertEqual "" (-150) $ waxOff (-50)
      assertEqual "" (triple waxOn) $ waxOff waxOnF
