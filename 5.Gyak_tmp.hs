{- ELTE-IK FunckProg, 5. gyakorlat -}
import Data.List

{- 1. Definiálj egyisPrimefüggvényt, mely megvizsgálja egy természetes számról, hogy prím-e! -}
isPrime :: Int -> Bool
isPrime a = [1, a] == [ b | b <- [1..a], a `mod` b == 0 ]

{- 2. Definiálj egyprimeskonstanst, amely prímek végtelen listáját tárolja -}
primes = [ a | a <- [1..], isPrime a ]

{- 3. Vizsgáld meg, hogy egy lista csak pozitív számokat tartalmaz-e! Érdemes bevetni a null függvényt a hatékonyság miatt. -}
--allPositive :: [Int] -> Bool
--allPositive [] = True
--allPositive (firstElem:rest)
--  | firstElem >= 0 = allPositive rest
--  | otherwise = False

allPositive :: [Int] -> Bool
allPositive [] = False
allPositive list = null [ a | a <- list, a < 0]

{- 4. Állítsd elő azt a listát, amely párokként tartalmazza az összes dominót:
  [(0,0), (0,1), ..., (0,6), (1,1), ..., (6, 6)] -}

dominoes = [ (a, b) | a <- [0..6], b <- [0..6] ] --TODO!
