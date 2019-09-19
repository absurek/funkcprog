Module FunctionDefinitions where

import Prelude hiding (even, odd)

mountain :: Integer -> [Integer]
mountain n = [1..n-1] ++ [n, n - 1..1]

areTriangleSides :: Real a => a -> a -> a -> Bool
areTriangleSides a b c = (a + b > c) && (a + c > b) && (b + c > a)

even :: Integer -> Bool
even i = i `mod` 2 == 0

odd :: Integer -> Bool
odd i = i `mod` 2 /= 0

divides :: Integer -> Integer -> Bool
divides a b = b `mod` a == 0

isLeapYear :: Integer -> Bool
isLeapYear year
  | (year `mod` 4 /= 0) = False
  | (year `mod` 100 /= 0) = True
  | (year `mod` 400 /= 0) = False
  | otherwise = True

sumSquaresTo :: Integer -> Integer
sumSquaresTo n = sum [ x * x | x <- [0..n] ]

divisors :: Integer -> [Integer]
divisors i = [ x | x <- [1..i], divides x i]

properDivisors :: Integer -> [Integer]
properDivisors i = [ x | x <- [2..i-1], divides x i]

{- See: http://lambda.inf.elte.hu/Definitions.xml -}
