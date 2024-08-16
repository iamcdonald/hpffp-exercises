module Main where

import Sing
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "Chapter v"
    [ writeATypeSignatureSuite,
      givenATypeWriteAFunctionSuite,
      fixItSuite
    ]

writeATypeSignatureSuite :: TestTree
writeATypeSignatureSuite =
  testGroup
    "Write a type signature"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three
    ]
  where
    one :: Assertion
    one = assertEqual "" 'x' $ functionH "xasd"
      where
        functionH :: [a] -> a
        functionH (x : _) = x
    two :: Assertion
    two = assertEqual "" True $ functionC 12 3
      where
        functionC :: (Ord a) => a -> a -> Bool
        functionC x y = if (x > y) then True else False
    three :: Assertion
    three = assertEqual "" 's' $ functionS ('x', 's')
      where
        functionS :: (x, y) -> y
        functionS (x, y) = y

givenATypeWriteAFunctionSuite :: TestTree
givenATypeWriteAFunctionSuite =
  testGroup
    "Given a type write a function"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three,
      testCase "4." four,
      testCase "5." five,
      testCase "6." six,
      testCase "7." seven,
      testCase "8." eight
    ]
  where
    one :: Assertion
    one = assertEqual "" 'a' $ i 'a'
      where
        i :: a -> a
        i x = x
    two :: Assertion
    two = assertEqual "" 12 $ c 12 '2'
      where
        c :: a -> b -> a
        c x y = x
    three :: Assertion
    three = assertEqual "" 12 $ c'' 12 '2'
      where
        c'' :: b -> a -> b
        c'' x y = x
    four :: Assertion
    four = assertEqual "" '2' $ c' 12 '2'
      where
        c' :: a -> b -> b
        c' x y = y
    five :: Assertion
    five = assertEqual "" "as" $ r "asdf"
      where
        r :: [a] -> [a]
        r = take 2
    six :: Assertion
    six = assertEqual "" '2' $ co last show 12
      where
        co :: (b -> c) -> (a -> b) -> a -> c
        co x y z = x (y z)
    seven :: Assertion
    seven = assertEqual "" 12 $ a show 12
      where
        a :: (a -> c) -> a -> a
        a x y = y
    eight :: Assertion
    eight = assertEqual "" 12 $ a show 12
      where
        a :: (a -> c) -> a -> a
        a x y = y

fixItSuite :: TestTree
fixItSuite =
  testGroup
    "Fix it"
    [ testCase "1." one
    ]
  where
    one :: Assertion
    one = do
      assertEqual "" "Singin in the rain" sing
