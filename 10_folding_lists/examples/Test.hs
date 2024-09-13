module Main where

import Data.Time
import Database
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain suite

data Dummy = Dummy deriving (Eq)

suite :: TestTree
suite =
  testGroup
    "Chapter 10 - Folding Lists"
    [ understandingFoldsSuite,
      databaseProcessingSuite,
      scansSuite,
      reviewSuite,
      rewriteAsFoldSuite
    ]

understandingFoldsSuite :: TestTree
understandingFoldsSuite =
  testGroup
    "Understanding Folds"
    [ testCase "1." one,
      testCase "2." two,
      testCase "5." five
    ]
  where
    one :: Assertion
    one =
      assertEqual "" (foldr (*) 1 [1 .. 5]) $ foldl (flip (*)) 1 [1 .. 5]
    two :: Assertion
    two =
      assertEqual "" (foldl (flip (*)) 1 [1 .. 3]) (3 * (2 * (1 * 1)))
    five :: Assertion
    five = do
      assertEqual "" "wootWOOTwoot" $ foldr (++) "" ["woot", "WOOT", "woot"]
      assertEqual "" 't' $ foldr max 'a' "fear is the little death"
      assertEqual "" False $ foldr (&&) True [False, True]
      assertEqual "" True $ foldr (||) True [False, True]
      assertEqual "" False $ foldr (||) False [False, False]
      assertEqual "" "54321" $ foldl (flip $ (++) . show) "" [1 .. 5]
      assertEqual "" 'a' $ foldr (flip const) 'a' [1 .. 5]
      assertEqual "" 0 $ foldr (flip const) 0 "tacos"
      assertEqual "" 0 $ foldl const 0 "burritos"
      assertEqual "" 'z' $ foldl const 'z' [1 .. 5]

databaseProcessingSuite :: TestTree
databaseProcessingSuite =
  testGroup
    "Database Processing"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three,
      testCase "4." four,
      testCase "5." five
    ]
  where
    one :: Assertion
    one =
      assertEqual "" expected $ filterDbDate theDatabase
      where
        filterDbDate :: [DatabaseItem] -> [UTCTime]
        filterDbDate = foldr f []
          where
            f (DbDate a) b = a : b
            f _ b = b
        expected =
          [ UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123),
            UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)
          ]
    two :: Assertion
    two =
      assertEqual "" expected $ filterDbNumber theDatabase
      where
        filterDbNumber :: [DatabaseItem] -> [Integer]
        filterDbNumber = foldr f []
          where
            f (DbNumber a) b = a : b
            f _ b = b
        expected = [9001, 9005]
    three :: Assertion
    three =
      assertEqual "" expected $ mostRecent theDatabase
      where
        mostRecent :: [DatabaseItem] -> UTCTime
        mostRecent = foldr f base
          where
            f (DbDate a) b = max a b
            f _ b = b
            base = UTCTime (fromGregorian 0 1 1) (secondsToDiffTime 0)
        expected = UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)
    four :: Assertion
    four =
      assertEqual "" expected $ sumDb theDatabase
      where
        sumDb :: [DatabaseItem] -> Integer
        sumDb = foldr f 0
          where
            f (DbNumber a) b = a + b
            f _ b = b
        expected = 18006
    five :: Assertion
    five =
      assertEqual "" expected $ avgDb theDatabase
      where
        avgDb :: [DatabaseItem] -> Double
        avgDb db = fromIntegral total / fromIntegral count
          where
            (total, count) = foldr f (0, 0) db
              where
                f (DbNumber a) (s, c) = (s + a, c + 1)
                f _ x = x
        expected = 9003.0

scansSuite :: TestTree
scansSuite =
  testGroup
    "Scans"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three
    ]
  where
    fibs = 1 : scanl (+) 1 fibs
    one :: Assertion
    one =
      assertEqual "" expected $ take 20 fibs
      where
        expected = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765]
    two :: Assertion
    two =
      assertEqual "" expected $ takeWhile (< 100) fibs
      where
        expected = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
    three :: Assertion
    three =
      assertEqual "" expected $ take 10 fac
      where
        expected = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
        fac = 1 : scanl (*) 1 [2 ..]

reviewSuite :: TestTree
reviewSuite =
  testGroup
    "Review"
    [ testCase "1.a" oneA,
      testCase "1.b" oneB,
      testCase "1.c" oneC,
      testCase "2." two,
      testCase "3." three
    ]
  where
    isSubset :: (Eq a) => [a] -> [a] -> Bool
    isSubset [] _ = True
    isSubset (c : cs) xs = c `elem` xs && isSubset cs xs

    oneA :: Assertion
    oneA = do
      assertEqual "" True $ isSubset expectedSub $ combo stops vowels
      assertEqual "" 180 $ length $ combo stops vowels
      where
        stops = "pbtdkg"
        vowels = "aeiou"
        expectedSub =
          [ "pab",
            "bid",
            "tug",
            "dep",
            "kot",
            "gak"
          ]
        combo :: String -> String -> [String]
        combo sts vows = [[s1, v, s2] | s1 <- sts, v <- vows, s2 <- sts]
    oneB :: Assertion
    oneB = do
      assertEqual "" True $ isSubset expectedSub $ comboP stops vowels
      assertEqual "" 30 $ length $ comboP stops vowels
      where
        stops = "pbtdkg"
        vowels = "aeiou"
        expectedSub =
          [ "pab",
            "pet",
            "pig",
            "pop",
            "puk",
            "pad"
          ]
        comboP sts vows = [[s1, v, s2] | s1 <- sts, v <- vows, s2 <- sts, s1 == 'p']
    oneC :: Assertion
    oneC = do
      assertEqual "" True $ isSubset expecteds $ combo nouns verbs
      assertEqual "" 27 $ length $ combo nouns verbs
      where
        nouns = ["door", "table", "television"]
        verbs = ["exist", "think", "eat"]
        expecteds =
          [ "door exist door",
            "door think table",
            "door eat television",
            "table think door",
            "table eat table",
            "table exist television",
            "television eat door",
            "television exist table",
            "television think television"
          ]
        combo ns vs = [n1 ++ " " ++ v ++ " " ++ n2 | n1 <- ns, v <- vs, n2 <- ns]
    two :: Assertion
    two =
      -- `length`` prevides Integers
      -- `div` then truncates when dividing using those ints
      -- hence we end up with 26/5 = 5 (rather than 5.2)
      assertEqual "" 5 $ seekritFunc "another wistful chain of words"
      where
        seekritFunc :: String -> Int
        seekritFunc x =
          div
            (sum (map length (words x)))
            (length (words x))
    three :: Assertion
    three =
      assertEqual "" 5.2 $ seekritFunc "another wistful chain of words"
      where
        seekritFunc :: String -> Double
        seekritFunc x =
          fromIntegral (sum (map length (words x)))
            / (fromIntegral (length (words x)))

rewriteAsFoldSuite :: TestTree
rewriteAsFoldSuite =
  testGroup
    "Rewrite as fold"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three,
      testCase "4." four,
      testCase "5." five,
      testCase "6." six,
      testCase "7." seven,
      testCase "8." eight,
      testCase "9." nine,
      testCase "10." ten,
      testCase "11." eleven
    ]
  where
    one :: Assertion
    one = do
      assertEqual "" False $ myOr [False, False]
      assertEqual "" True $ myOr [False, True]
      where
        myOr :: [Bool] -> Bool
        myOr = foldr (||) False
    two :: Assertion
    two = do
      assertEqual "" False $ myAny even [3, 9]
      assertEqual "" True $ myAny even [2, 9]
      where
        myAny :: (a -> Bool) -> [a] -> Bool
        myAny f = foldr wf False
          where
            wf a b = f a || b
    three :: Assertion
    three = do
      assertEqual "" True $ myElem 4 [1 .. 10]
      assertEqual "" False $ myElem 0 [1 .. 10]
      assertEqual "" True $ myElemAny 4 [1 .. 10]
      assertEqual "" False $ myElemAny 0 [1 .. 10]
      where
        myElem :: (Eq a) => a -> [a] -> Bool
        myElem e = foldr check False
          where
            check a b = a == e || b
        myElemAny :: (Eq a) => a -> [a] -> Bool
        myElemAny e = any (== e)
    four :: Assertion
    four =
      assertEqual "" (reverse "aeiou") $ myReverse "aeiou"
      where
        myReverse :: [a] -> [a]
        myReverse = foldr (\a b -> b ++ [a]) []
    five :: Assertion
    five =
      assertEqual "" (map (+ 1) [1 .. 3]) $ myMap (+ 1) [1 .. 3]
      where
        myMap :: (a -> b) -> [a] -> [b]
        myMap f = foldr (\a b -> f a : b) []
    six :: Assertion
    six =
      assertEqual "" (filter even [1 .. 10]) $ myFilter even [1 .. 10]
      where
        myFilter :: (a -> Bool) -> [a] -> [a]
        myFilter f = foldr filt []
          where
            filt a b = if f a then a : b else b
    seven :: Assertion
    seven =
      assertEqual "" [1, 2, 3] $ squish [[1], [2], [3]]
      where
        squish :: [[a]] -> [a]
        squish = foldr (++) []
    eight :: Assertion
    eight =
      assertEqual "" [1, 2, 3] $ squishMap (\x -> [1, x, 3]) [2]
      where
        squishMap :: (a -> [b]) -> [a] -> [b]
        squishMap f = foldr (\a b -> f a ++ b) []
    nine :: Assertion
    nine =
      assertEqual "" [1, 2, 3] $ squishAgain [[1], [2], [3]]
      where
        squishMap :: (a -> [b]) -> [a] -> [b]
        squishMap f = foldr (\a b -> f a ++ b) []
        squishAgain :: [[a]] -> [a]
        squishAgain = squishMap id
    ten :: Assertion
    ten = do
      assertEqual "" 1 $ myMaximumBy (\_ _ -> GT) [1 .. 10]
      assertEqual "" 10 $ myMaximumBy (\_ _ -> LT) [1 .. 10]
      assertEqual "" 10 $ myMaximumBy compare [1 .. 10]
      where
        myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
        myMaximumBy _ [] = undefined
        myMaximumBy f (x : xs) = foldl check x xs
          where
            check a b
              | f a b == GT = a
              | otherwise = b
    eleven :: Assertion
    eleven = do
      assertEqual "" 10 $ myMinimumBy (\_ _ -> GT) [1 .. 10]
      assertEqual "" 1 $ myMinimumBy (\_ _ -> LT) [1 .. 10]
      assertEqual "" 1 $ myMinimumBy compare [1 .. 10]
      where
        myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
        myMinimumBy _ [] = undefined
        myMinimumBy f (x : xs) = foldl check x xs
          where
            check a b
              | f a b == LT = a
              | otherwise = b
