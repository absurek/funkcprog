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
dominoes = [ (a, b) | a <- [0..6], b <- [0..6], a >= b ]

{-5. *Sorold fel az összes természetes számpárt:
      [(0,0),(0,1),(1,0),(0,2),(1,1),(2,0), ... -}
--allIntPairs = [ (a, b) | a <- [0..], b <- [0..], a >= b] -- TODO!

{- 6. Sorszámozd meg az angol ábécé betűit! -}
alphabet = zip [1..] ['a'..'z']

{- 7. Gyűjtsd ki minden harmadik betűt az angol ábécéből egy listakifejezéssel!-}
everyThird = [ c | (i, c) <- alphabet, i `mod` 3 == 2]

{- 8. Neptunban a tárgyakat egy listában tároljuk. Minden tárgynak van neve és feljelentkezett diákjai: -}

courses =[ ("Programozasi nyelvek II.", [("Horvath", "Istvan", "BDE91E"), ("Fodros", "Aniko", "DDA3KX")]), ("Imperativ programozas",    [("Nemeth", "Eniko", "ALX1K0"), ("Horvath", "Istvan", "BDE91E")]), ("Funkcionalis programozas", [("Kiss", "Elemer", "ABCDE6"), ("Nagy", "Jakab", "CDE560")])]

{- Egyetlen listakifejezéssel sorold fel a Funkcionális programozást felvett diákok Neptun-kódját! Nem ér indexelést használni, nem tehetünk fel semmit a tárgyak helyéről a listában.

students == ["ABCDE6", "CDE560"]

Általánosítsd a kódot függvénnyé, azaz várd paraméterül a keresett tárgy nevét! -}
listAllStudentsOfCourse :: String -> [(String, [(String, String, String)])] -> [String]
listAllStudentsOfCourse courseName courseList =
  [ neptunCode | (course, studentList) <- courseList, (_, _, neptunCode) <- studentList, course == courseName ]

{- 9. *Állítsuk elő azt a listát, amely sorrendben tartalmazza az összes (hónap, nap) párt egy 365 napos évben! Tipp: használjuk az elem :: a -> [a] -> Bool függvényt! -}
everyDay = [ (month, day) | month <- ["J", "Mar", "May", "Jl", "Au", "O", "D"], day <- [1..31] ]
           ++ [ (month, day) | month <- ["Ap", "Ju", "S", "N"], day <- [1..30]]
           ++ [ ("F", day) | day <- [1..28] ]

{- 10, 11 Hfként beadva. -}
