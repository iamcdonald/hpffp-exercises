module Main where

import EqInstances
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain suite

data Dummy = Dummy deriving (Eq)

suite :: TestTree
suite =
  testGroup
    "Chapter 6 - Type Classes"
    [ eqInstancesSuite,
      typeKwonDoSuite
    ]

eqInstancesSuite :: TestTree
eqInstancesSuite =
  testGroup
    "Eq Instances"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three,
      testCase "4." four,
      testCase "5." five,
      testCase "6." six,
      testCase "7." seven
    ]
  where
    one :: Assertion
    one = do
      assertEqual "" True $ TisAn 12 == TisAn 12
      assertEqual "" False $ TisAn 12 == TisAn 11
    two :: Assertion
    two = do
      assertEqual "" True $ Two 12 5 == Two 12 5
      assertEqual "" False $ Two 1 5 == Two 12 5
    three :: Assertion
    three = do
      assertEqual "" True $ TisAString "string" == TisAString "string"
      assertEqual "" False $ TisAString "hello" == TisAString "string"
      assertEqual "" True $ TisAnInt 12 == TisAnInt 12
      assertEqual "" False $ TisAnInt 11 == TisAnInt 12
      assertEqual "" False $ TisAnInt 12 == TisAString "string"
    four :: Assertion
    four = do
      assertEqual "" True $ Pair "s" "t" == Pair "s" "t"
      assertEqual "" False $ Pair 2 2 == Pair 2 3
    five :: Assertion
    five = do
      assertEqual "" True $ Tuple "s" 12 == Tuple "s" 12
      assertEqual "" False $ Tuple 12 "s" == Tuple 12 "d"
    six :: Assertion
    six = do
      assertEqual "" True $ ThisOne 'c' == ThisOne 'c'
      assertEqual "" False $ ThisOne "s" == ThisOne "st"
      assertEqual "" True $ ThatOne "s" == ThatOne "s"
      assertEqual "" False $ ThatOne 12 == ThatOne 11
      assertEqual "" False $ ThisOne 12 == ThatOne 12
    seven :: Assertion
    seven =
      do
        assertEqual "" True $ Hello "2" == (Hello "2" :: EitherOr String Dummy)
        assertEqual "" False $ Hello 1 == (Hello 2 :: EitherOr Int Dummy)
        assertEqual "" True $ Goodbye 12 == (Goodbye 12 :: EitherOr Dummy Int)
        assertEqual "" False $ Goodbye "bye" == (Goodbye "boy" :: EitherOr Dummy String)
        assertEqual "" False $ (Goodbye 12 :: EitherOr Int Int) == (Hello 12 :: EitherOr Int Int)

typeKwonDoSuite :: TestTree
typeKwonDoSuite =
  testGroup
    "Type-Kwon-Do"
    [ testCase "1." one,
      testCase "2." two
    ]
  where
    one :: Assertion
    one = do
      assertEqual "" True $ chk show 10 "10"
      where
        chk :: (Eq b) => (a -> b) -> a -> b -> Bool
        chk f a b = f a == b
    two :: Assertion
    two = do
      assertEqual "" 15 $ arith toInteger (12 :: Integer) (6 `div` 2)
      where
        arith :: (Num b) => (a -> b) -> Integer -> a -> b
        arith f i a = fromInteger i + f a
