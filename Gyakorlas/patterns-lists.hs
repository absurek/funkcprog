module PatternsAndLists where

import Prelude hiding ((||))
import Data.Char

(||) :: Bool -> Bool -> Bool
(||) a b
  | a == True = True
  | b == True = True
  | otherwise = False

xor :: Bool -> Bool -> Bool
a `xor` b
  | (a == True && b == True) || (a == False && b == False) = False
  | otherwise = True

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

mirrorX :: Num a => (a, a) -> (a, a)
mirrorX (x, y) = (x, -y)

scale' :: Num a => a -> (a, a) -> (a, a)
scale' n (x, y) = (n*x, n*y)

mirrorP :: Num a => (a, a) -> (a, a) -> (a, a)
mirrorP (x, y) (z, k) = (2 * x - z, 2 * y - k)

distance :: Floating t => (t, t) -> (t, t) -> t
distance (x, y) (z, k) = sqrt ((x - z) * (x - z) + (y - k) * (y - k))

mul3 :: Int -> Int -> Int
a `mul3` b
  | (a * b) `mod` 3 == 0 = 0
  | (a * b) `mod` 3 == 1 = 1
  | (a * b) `mod` 3 == 2 = 2

replaceNewline :: Char -> Char
replaceNewline theChar
  | theChar == '\n' = ' '
  | otherwise = theChar

replaceNewlines :: String -> String
replaceNewlines theString = [ replaceNewline theChar | theChar <- theString ]

swap_a_az :: String -> String
swap_a_az theString
  | theString == "a"  = "az"
  | theString == "az" = "a"
  | otherwise = theString

swapAll_a_az :: String -> String
swapAll_a_az theStirng = unwords [ swap_a_az word | word <- words theStirng ]

toUpperFirst :: String -> String
toUpperFirst (firstLetter:restOfTheString) = (toUpper firstLetter):restOfTheString

toUpperFirsts :: String -> String
toUpperFirsts theString = unwords [ toUpperFirst word | word <- words theString ]

isSingleton :: [a] -> Bool
isSingleton theList
  | null theList = False
  | null (tail theList) = True
  | otherwise = False

countOfAs :: String -> Int
countOfAs theString = sum [ 1 | word <- words theString, word == "a" ]

distantPairs :: [(Integer,Integer)] -> Int
distantPairs pairList = sum [ 1 | (a, b) <- pairList, abs (a - b) >= 2 ]

everyFifth :: [a] -> [a]
everyFifth theList = [ a | (a, b) <- zip (theList) (take (length theList) (cycle [1..5])), b == 1 ]

{- See: http://lambda.inf.elte.hu/Patterns.xml -}
