module Main where

import Cipher
import Data.Bool
import Data.Char qualified as Char
import Data.List
import Data.Maybe
import PoemLines
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain suite

data Dummy = Dummy deriving (Eq)

suite :: TestTree
suite =
  testGroup
    "Chapter 9 - Lists"
    [ fearfulSymmetrySuite,
      comprehendThyListsSuite,
      squareCubeSuite,
      moreBottomsSuite,
      filteringSuite,
      zippingSuite,
      chapterExCharSuite,
      chapterExCipherSuite,
      chapterExStandardFuncSuite
    ]

fearfulSymmetrySuite :: TestTree
fearfulSymmetrySuite =
  testGroup
    "Fearful Symmetry"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three
    ]
  where
    one :: Assertion
    one =
      assertEqual "" ["sheryl", "wants", "fun"] $ myWords "sheryl wants fun"
      where
        myWords :: String -> [String]
        myWords [] = []
        myWords s = takeWhile (not . isSpace) s : myWords ((trim . dropWhile (not . isSpace)) s)
          where
            isSpace = (==) ' '
            trim = dropWhile isSpace
    two :: Assertion
    two =
      assertEqual "" expected $ myLines sentences
      where
        expected =
          [ "Tyger Tyger, burning bright",
            "In the forests of the night",
            "What immortal hand or eye",
            "Could frame thy fearful symmetry?"
          ]
    three :: Assertion
    three = do
      assertEqual "" ["sheryl", "wants", "fun"] $ myChunks ' ' "sheryl wants fun"
      assertEqual "" expected $ myChunks '\n' sentences
      where
        myChunks :: Char -> String -> [String]
        myChunks _ [] = []
        myChunks delim s =
          takeWhile (not . isDelim) s : myChunks delim ((dropWhile isDelim . dropWhile (not . isDelim)) s)
          where
            isDelim = (==) delim
        expected =
          [ "Tyger Tyger, burning bright",
            "In the forests of the night",
            "What immortal hand or eye",
            "Could frame thy fearful symmetry?"
          ]

comprehendThyListsSuite :: TestTree
comprehendThyListsSuite =
  testGroup
    "Comprehend thy lists"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three
    ]
  where
    mySqr = [x ^ 2 | x <- [1 .. 10]]
    -- [1,4,9,16,25,36,49,64,81,100]
    one :: Assertion
    one =
      assertEqual "" expected [x | x <- mySqr, rem x 2 == 0]
      where
        expected = [4, 16, 36, 64, 100]
    two :: Assertion
    two =
      assertEqual "" expected [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
      where
        expected =
          [ (1, 64),
            (1, 81),
            (1, 100),
            (4, 64),
            (4, 81),
            (4, 100),
            (9, 64),
            (9, 81),
            (9, 100),
            (16, 64),
            (16, 81),
            (16, 100),
            (25, 64),
            (25, 81),
            (25, 100),
            (36, 64),
            (36, 81),
            (36, 100),
            (49, 64),
            (49, 81),
            (49, 100)
          ]
    three :: Assertion
    three =
      assertEqual "" expected $ take 5 [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
      where
        expected =
          [ (1, 64),
            (1, 81),
            (1, 100),
            (4, 64),
            (4, 81)
          ]

squareCubeSuite :: TestTree
squareCubeSuite =
  testGroup
    "Square cube"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three
    ]
  where
    mySqr = [x ^ 2 | x <- [1 .. 5]]
    myCube = [y ^ 3 | y <- [1 .. 5]]
    one :: Assertion
    one =
      assertEqual "" expected mySqrCubeTuples
      where
        mySqrCubeTuples = [(x, y) | x <- mySqr, y <- myCube]
        expected =
          [ (1, 1),
            (1, 8),
            (1, 27),
            (1, 64),
            (1, 125),
            (4, 1),
            (4, 8),
            (4, 27),
            (4, 64),
            (4, 125),
            (9, 1),
            (9, 8),
            (9, 27),
            (9, 64),
            (9, 125),
            (16, 1),
            (16, 8),
            (16, 27),
            (16, 64),
            (16, 125),
            (25, 1),
            (25, 8),
            (25, 27),
            (25, 64),
            (25, 125)
          ]
    two :: Assertion
    two =
      assertEqual "" expected mySqrCubeTuples
      where
        mySqrCubeTuples = [(x, y) | x <- mySqr, y <- myCube, x < 50 && y < 50]
        expected =
          [ (1, 1),
            (1, 8),
            (1, 27),
            (4, 1),
            (4, 8),
            (4, 27),
            (9, 1),
            (9, 8),
            (9, 27),
            (16, 1),
            (16, 8),
            (16, 27),
            (25, 1),
            (25, 8),
            (25, 27)
          ]
    three :: Assertion
    three =
      assertEqual "" 15 mySqrCubeTuples
      where
        mySqrCubeTuples = length [(x, y) | x <- mySqr, y <- myCube, x < 50 && y < 50]

moreBottomsSuite :: TestTree
moreBottomsSuite =
  testGroup
    "More bottoms"
    [ testCase "4." four,
      testCase "5." five,
      testCase "6." six
    ]
  where
    four :: Assertion
    four =
      assertEqual "" expected $ itIsMystery "arteries"
      where
        itIsMystery xs = map (\x -> elem x "aeiou") xs
        expected = [True, False, False, True, False, True, True, False]
    five :: Assertion
    five = do
      assertEqual "" expectedA $ map (^ 2) [1 .. 10]
      assertEqual "" expectedB $ map minimum [[1 .. 10], [10 .. 20], [20 .. 30]]
      assertEqual "" expectedC $ map sum [[1 .. 5], [1 .. 5], [1 .. 5]]
      where
        expectedA = [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
        expectedB = [1, 10, 20]
        expectedC = [15, 15, 15]
    six :: Assertion
    six =
      assertEqual "" expected $ map (\x -> bool x (-x) (x == 3)) [1 .. 10]
      where
        expected = [1, 2, -3, 4, 5, 6, 7, 8, 9, 10]

filteringSuite :: TestTree
filteringSuite =
  testGroup
    "Filtering"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three
    ]
  where
    one :: Assertion
    one =
      assertEqual "" expected $ filter ((== 0) . flip rem 3) [1 .. 30]
      where
        expected = [3, 6, 9, 12, 15, 18, 21, 24, 27, 30]
    two :: Assertion
    two =
      assertEqual "" 10 $ (length . threes) [1 .. 30]
      where
        threes = filter ((== 0) . flip rem 3)
    three :: Assertion
    three =
      assertEqual "" ["brown", "dog", "was", "goof"] $ myFilter "the brown dog was a goof"
      where
        myFilter :: String -> [String]
        myFilter = filter (not . flip elem ["the", "a", "an"]) . words

zippingSuite :: TestTree
zippingSuite =
  testGroup
    "Zipping"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three
    ]
  where
    one :: Assertion
    one =
      assertEqual "" (zip al bl) $ myZip al bl
      where
        al = [1 .. 10]
        bl = [11 .. 15]
        myZip :: [a] -> [b] -> [(a, b)]
        myZip (a : as) (b : bs) = (a, b) : myZip as bs
        myZip _ _ = []
    two :: Assertion
    two =
      assertEqual "" (zipWith (/) al bl) $ myZipWith (/) al bl
      where
        al = [1 .. 10]
        bl = [11 .. 15]
        myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
        myZipWith op (a : as) (b : bs) = op a b : myZipWith op as bs
        myZipWith _ _ _ = []
    three :: Assertion
    three =
      assertEqual "" (zip al bl) $ myZip al bl
      where
        al = [1 .. 10]
        bl = [23 .. 80]
        myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
        myZipWith op (a : as) (b : bs) = op a b : myZipWith op as bs
        myZipWith _ _ _ = []
        myZip = myZipWith (,)

chapterExCharSuite :: TestTree
chapterExCharSuite =
  testGroup
    "Chapter Exercises - Data.Char"
    [ testCase "2." two,
      testCase "3." three,
      testCase "4." four,
      testCase "5." five,
      testCase "6." six
    ]
  where
    two :: Assertion
    two =
      assertEqual "" "HELLO" $ filter Char.isUpper "HbEfLrLxO"
    three :: Assertion
    three =
      assertEqual "" "Julie" $ capitalize "julie"
      where
        capitalize (x : xs) = Char.toUpper x : xs
        capitalize [] = []
    four :: Assertion
    four =
      assertEqual "" "WOOT" $ capitals "woot"
      where
        capitals (x : xs) = Char.toUpper x : capitals xs
        capitals [] = []
    five :: Assertion
    five =
      assertEqual "" 'J' $ Char.toUpper (head "julie")
    six :: Assertion
    six = do
      assertEqual "" 'J' $ capitalComp "julie"
      assertEqual "" 'J' $ capitalPointFree "julie"
      where
        capitalComp :: String -> Char
        capitalComp x = Char.toUpper (head x)
        capitalPointFree :: String -> Char
        capitalPointFree = Char.toUpper . head

chapterExCipherSuite :: TestTree
chapterExCipherSuite =
  testGroup
    "Chapter Exercises - Cipher"
    [ testCase "1." one
    ]
  where
    one :: Assertion
    one = do
      assertEqual "" "Ifmmp1" $ ceaser 1 "Hello1"
      assertEqual "" "Hello1" $ unCeaser 1 $ ceaser 1 "Hello1"

chapterExStandardFuncSuite :: TestTree
chapterExStandardFuncSuite =
  testGroup
    "Chapter Exercises - Standard Functions"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three,
      testCase "4." four,
      testCase "5." five,
      testCase "6." six,
      testCase "7." seven,
      testCase "8." eight,
      testCase "9." nine,
      testCase "10." ten
    ]
  where
    one :: Assertion
    one = do
      assertEqual "" (or [True, False]) $ myOr [True, False]
      assertEqual "" (or [False, False]) $ myOr [False, False]
      assertEqual "" (or [True, True]) $ myOr [True, True]
      where
        myOr :: [Bool] -> Bool
        myOr = isJust . find id
    two :: Assertion
    two = do
      assertEqual "" (any even [1, 10]) $ myAny even [1, 10]
      assertEqual "" False $ myAny even [1, 3, 5]
      assertEqual "" True $ myAny odd [1, 3, 5]
      where
        myAny :: (a -> Bool) -> [a] -> Bool
        myAny op = isJust . find op
    three :: Assertion
    three = do
      assertEqual "" (elem 2 [2, 4]) $ myElem 2 [2, 4]
      assertEqual "" True $ myElem 1 [1 .. 10]
      assertEqual "" False $ myElem 1 [2 .. 10]
      where
        myElem :: (Eq a) => a -> [a] -> Bool
        myElem a = isJust . find (== a)
    four :: Assertion
    four = do
      assertEqual "" "olleh" $ myReverse "hello"
      assertEqual "" [5, 4, 3, 2, 1] $ myReverse [1, 2, 3, 4, 5]
      where
        myReverse :: [a] -> [a]
        myReverse = rev []
          where
            rev :: [a] -> [a] -> [a]
            rev c [] = c
            rev c (x : xs) = rev (x : c) xs
    five :: Assertion
    five =
      assertEqual "" [1, 2, 3, 4] $ squish [[1, 2], [], [3], [4]]
      where
        squish :: [[a]] -> [a]
        squish [] = []
        squish (x : xs) = x ++ squish xs
    six :: Assertion
    six = do
      assertEqual "" [1, 2, 3] $ squishMap (\x -> [1, x, 3]) [2]
      assertEqual "" "WO 1 HOO WO 2 HOO WO 3 HOO " $ squishMap (\x -> "WO " ++ [x] ++ " HOO ") "123"
      where
        squishMap :: (a -> [b]) -> [a] -> [b]
        squishMap _ [] = []
        squishMap op (x : xs) = op x ++ squishMap op xs
    seven :: Assertion
    seven =
      assertEqual "" [1, 2, 3, 4] $ squishAgain [[1, 2], [], [3], [4]]
      where
        squishMap :: (a -> [b]) -> [a] -> [b]
        squishMap _ [] = []
        squishMap op (x : xs) = op x ++ squishMap op xs
        squishAgain = squishMap id
    eight :: Assertion
    eight =
      assertEqual "" 9001 $ myMaximumBy compare [1, 53, 9001, 10]
      where
        myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
        myMaximumBy op = last . sortBy op
    nine :: Assertion
    nine =
      assertEqual "" 9 $ myMinimumBy compare [9, 53, 9001, 10]
      where
        myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
        myMinimumBy op = head . sortBy op
    ten :: Assertion
    ten = do
      assertEqual "" 9 $ myMinimum [9, 53, 9001, 10]
      assertEqual "" 9001 $ myMaximum [9, 53, 9001, 10]
      where
        myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
        myMinimumBy op = head . sortBy op
        myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
        myMaximumBy op = last . sortBy op
        myMaximum :: (Ord a) => [a] -> a
        myMaximum = myMaximumBy compare
        myMinimum :: (Ord a) => [a] -> a
        myMinimum = myMinimumBy compare
