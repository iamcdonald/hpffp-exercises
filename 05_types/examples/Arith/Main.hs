module Main where

main :: IO ()
main = do
  print (1 + 2)
  print 10
  print (negate 1 :: Integer)
  print ((+) 0 blah)
  where
    blah = negate 1
