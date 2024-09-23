module Main where

import BinaryTree
import Data.Char
import Data.List
import Data.Maybe
import Data.Typeable
import HuttonsRazor
import Phone
import Programmers
import Test.Tasty
import Test.Tasty.HUnit
import TooMany
import Vehicles

main :: IO ()
main = defaultMain suite

data Dummy = Dummy deriving (Eq)

suite :: TestTree
suite =
  testGroup
    "Chapter 11 - Algebraic Datatypes"
    [ vehiclesSuite,
      logicGoatsSuite,
      programmersSuite,
      binaryTreeSuite,
      cipherSuite,
      asPatternsSuite,
      languageExercisesSuite,
      phoneExerciseSuite,
      huttonsRazorSuite
    ]

vehiclesSuite :: TestTree
vehiclesSuite =
  testGroup
    "Vehicles"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three,
      testCase "5." five
    ]
  where
    myCar = Car Mini (Price 14000)
    urCar = Car Mazda (Price 20000)
    clownCar = Car Tata (Price 7000)
    doge = Plane PapuAir
    one :: Assertion
    one =
      assertEqual "" "Vehicle" $ show $ typeOf myCar
    two :: Assertion
    two = do
      assertEqual "" True $ isCar myCar
      assertEqual "" False $ isCar doge
      assertEqual "" False $ isPlane myCar
      assertEqual "" True $ isPlane doge
      assertEqual "" [True, False, True, True] $ areCars [myCar, doge, urCar, clownCar]
      where
        isCar :: Vehicle -> Bool
        isCar x
          | (Car _ _) <- x = True
          | otherwise = False
        isPlane :: Vehicle -> Bool
        isPlane x
          | (Plane _) <- x = True
          | otherwise = False
        areCars :: [Vehicle] -> [Bool]
        areCars = map isCar
    three :: Assertion
    three = do
      assertEqual "" Mini $ getManu myCar
      assertEqual "" Tata $ getManu clownCar
      where
        getManu :: Vehicle -> Manufacturer
        getManu (Car m _) = m
    -- four -> will throw error as getManu doesn't provide pattern to handle Plane'
    five :: Assertion
    five = do
      assertEqual "" True $ isPlane doge2
      assertEqual "" False $ isPlane clownCar
      where
        clownCar = Car2 Tata (Price 7000)
        doge2 = Plane2 PapuAir (Size 10 12)
        isPlane :: Vehicle2 -> Bool
        isPlane x
          | (Plane2 _ _) <- x = True
          | otherwise = False

logicGoatsSuite :: TestTree
logicGoatsSuite =
  testGroup
    "Logic Goats"
    [ testCase "1." one,
      testCase "2." two,
      testCase "3." three
    ]
  where
    one :: Assertion
    one = do
      assertEqual "" True $ tooMany (51 :: Int, "Hello")
      assertEqual "" False $ tooMany (28 :: Int, "Hello")
    two :: Assertion
    two = do
      assertEqual "" True $ tooMany (29 :: Int, 21 :: Int)
      assertEqual "" False $ tooMany (28 :: Int, 6 :: Int)
    three :: Assertion
    three = do
      -- changing type to stop overlap with previous addition instance of TooMany (Int, Int)
      assertEqual "" True $ tooMany (50 :: Int, 61 :: Int, 0 :: Int)
      assertEqual "" False $ tooMany (21 :: Int, 44 :: Int, 0 :: Int)

programmersSuite :: TestTree
programmersSuite =
  testGroup
    "Programmers"
    [ testCase "1." one
    ]
  where
    one :: Assertion
    one = do
      assertEqual "" expected allProgrammers
      assertEqual "" (length allOperatingSystems * length allLanguages) $ length allProgrammers
      where
        expected =
          [ Programmer GnuPlusLinux Haskell,
            Programmer GnuPlusLinux Agda,
            Programmer GnuPlusLinux Idris,
            Programmer GnuPlusLinux PureScript,
            Programmer OpenBSDPlusNevermindJustBSDStill Haskell,
            Programmer OpenBSDPlusNevermindJustBSDStill Agda,
            Programmer OpenBSDPlusNevermindJustBSDStill Idris,
            Programmer OpenBSDPlusNevermindJustBSDStill PureScript,
            Programmer Mac Haskell,
            Programmer Mac Agda,
            Programmer Mac Idris,
            Programmer Mac PureScript,
            Programmer Windows Haskell,
            Programmer Windows Agda,
            Programmer Windows Idris,
            Programmer Windows PureScript
          ]

binaryTreeSuite :: TestTree
binaryTreeSuite =
  testGroup
    "Binary Tree"
    [ testCase "map" btMap,
      testCase "to list" btToList,
      testCase "foldr" btFoldr
    ]
  where
    btMap :: Assertion
    btMap =
      assertEqual "" expected $ mapTree (+ 1) testTree'
      where
        testTree' :: BinaryTree Integer
        testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
        expected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
    btToList :: Assertion
    btToList = do
      assertEqual "" expectedInorder $ inorder testTree
      assertEqual "" expectedPreorder $ preorder testTree
      assertEqual "" expectedPostorder $ postorder testTree
      where
        testTree :: BinaryTree Integer
        testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
        expectedInorder = [1, 2, 3]
        expectedPreorder = [2, 1, 3]
        expectedPostorder = [1, 3, 2]
    btFoldr :: Assertion
    btFoldr = do
      assertEqual "" 6 $ foldTree (+) 0 testTree
      assertEqual "" 9 $ foldTree (+) 3 testTree
      assertEqual "" 12 $ foldTree (*) 2 testTree
      where
        testTree :: BinaryTree Integer
        testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

cipherSuite :: TestTree
cipherSuite =
  testGroup
    "Ciphers"
    [ testCase "1." one
    ]
  where
    one :: Assertion
    one =
      assertEqual "" "MPPR AE OYWY" $ encode "ALLY" "MEET AT DAWN"
      where
        encode :: String -> String -> String
        encode enc str = zipWith shiftChar str $ shifts (cycle enc) str
          where
            alpha = ['A' .. 'Z']
            shifts :: String -> String -> String
            shifts _ [] = []
            shifts [] x = x
            shifts shs (' ' : xs) = ' ' : shifts shs xs
            shifts (sh : shs) (_ : xs) = sh : shifts shs xs
            shiftChar :: Char -> Char -> Char
            shiftChar ' ' _ = ' '
            shiftChar a s = alpha !! mod (fromJust (elemIndex a alpha) + (ord s - ord 'A')) (length alpha)

asPatternsSuite :: TestTree
asPatternsSuite =
  testGroup
    "As-Patterns"
    [ testCase "1." one,
      testCase "2." two
    ]
  where
    one :: Assertion
    one = do
      assertEqual "" True $ isSubseqOf "blah" "blahwoot"
      assertEqual "" True $ isSubseqOf "blah" "wootblah"
      assertEqual "" True $ isSubseqOf "blah" "wboloath"
      assertEqual "" False $ isSubseqOf "blah" "wootbla"
      where
        isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
        isSubseqOf [] _ = True
        isSubseqOf _ [] = False
        isSubseqOf xs ys = foldr (\x agg -> agg && x `elem` ys) True xs
    two :: Assertion
    two =
      assertEqual "" [("hello", "Hello"), ("world", "World")] $ capitalizeWords "hello world"
      where
        capitalizeWords :: String -> [(String, String)]
        capitalizeWords s = map (\w -> (w, capitalize w)) $ words s
          where
            capitalize :: String -> String
            capitalize [] = []
            capitalize (x : xs) = toUpper x : xs

languageExercisesSuite :: TestTree
languageExercisesSuite =
  testGroup
    "Language Exercises"
    [ testCase "1." one,
      testCase "2." two
    ]
  where
    one :: Assertion
    one = do
      assertEqual "" "Chortle" $ capitalizeWord "Chortle"
      assertEqual "" "Chortle" $ capitalizeWord "chortle"
      where
        capitalizeWord :: String -> String
        capitalizeWord [] = []
        capitalizeWord (x : xs) = toUpper x : xs
    two :: Assertion
    two =
      assertEqual "" "Blah. Woot ha." $ capitalizeParagraph "blah. woot ha."
      where
        capitalizeParagraph :: String -> String
        capitalizeParagraph [] = ""
        capitalizeParagraph p = concatMap capitalize sentances
          where
            sentances = foldl' process [""] p
              where
                process :: [String] -> Char -> [String]
                process agg '.' = init agg ++ [last agg ++ ['.']] ++ [""]
                process agg x = init agg ++ [last agg ++ [x]]
            capitalize :: String -> String
            capitalize [] = []
            capitalize (x : xs)
              | x == ' ' = x : capitalize xs
              | otherwise = toUpper x : xs

phoneExerciseSuite :: TestTree
phoneExerciseSuite =
  testGroup
    "Phone Exercise"
    [ testCase "2." two,
      testCase "3." three,
      testCase "4." four,
      testCase "5." five,
      testCase "6." six
    ]
  where
    two :: Assertion
    two = do
      assertEqual "" [('2', 1)] $ reverseTaps phone 'a'
      assertEqual "" [('*', 1), ('5', 2)] $ reverseTaps phone 'K'
      assertEqual "" expectedConvo $ cellPhonesDead phone "Wanna play 20 Questions"
      where
        expectedConvo =
          [ ('*', 1),
            ('9', 1), -- W
            ('2', 1), -- a
            ('6', 2), -- n
            ('6', 2), -- n
            ('2', 1), -- a
            ('0', 2), -- ' '
            ('7', 1), -- p
            ('5', 3), -- l
            ('2', 1), -- a
            ('9', 3), -- y
            ('0', 2), -- ' '
            ('2', 4), -- 2
            ('0', 3), -- 0
            ('0', 2), -- ' '
            ('*', 1),
            ('7', 2), -- Q
            ('8', 2), -- u
            ('3', 2), -- e
            ('7', 4), -- s
            ('8', 1), -- t
            ('4', 3), -- i
            ('6', 3), -- o
            ('6', 2), -- n
            ('7', 4) -- s
          ]
    three :: Assertion
    three = do
      assertEqual "" 1 $ fingerTaps $ reverseTaps phone 'a'
      assertEqual "" 8 $ fingerTaps $ cellPhonesDead phone "Wanna"
    four :: Assertion
    four = do
      assertEqual "" ('a', 3) $ mostPopularLetterCost "Wanna play Questions"
    five :: Assertion
    five =
      assertEqual "" 'o' $ coolestLtr convo
    six :: Assertion
    six =
      assertEqual "" "Lol" $ coolestWord convo

huttonsRazorSuite :: TestTree
huttonsRazorSuite =
  testGroup
    "Huttons Razor"
    [ testCase "1." one,
      testCase "2." two
    ]
  where
    one :: Assertion
    one =
      assertEqual "" 9002 $ eval (Add (Lit 1) (Lit 9001))
    two :: Assertion
    two = do
      assertEqual "" "1 + 9001" $ printExpr (Add (Lit 1) (Lit 9001))
      assertEqual "" "1 + 9001 + 1 + 20001" $ printExpr a3
      where
        a1 = Add (Lit 9001) (Lit 1)
        a2 = Add a1 (Lit 20001)
        a3 = Add (Lit 1) a2
