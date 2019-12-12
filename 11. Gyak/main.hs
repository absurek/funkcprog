{- 11. Gyakorlat -}
import Data.List

{- 1. Definiáld a dropSpaces függvényt, mely szóközöket dob el egyString elejéről!
Használj magasabb rendű függvényt. -}
dropSpaces :: String -> String
dropSpaces str = dropWhile (\c -> c == ' ') str

{- 2. Definiáld a trim függvényt, mely szóközöket dob el egy String mindkét végéről! -}
trim :: String -> String
trim str = reverse (dropSpaces (reverse (dropSpaces str)))

{- 3. Definiáld a monogram függvényt, mely egy név monogramját adja meg! Használd a
wordsöt és magasabb rendű függvényt. -}
monogram :: String -> String
--monogram names = trim (unwords [ x:'.':' ':[] | (x:xs) <- (words names) ])
monogram names = trim (unwords (map (\(x:xs) -> x:'.':[]) (words names)))

{- 4. Definiáld a uniq :: Ord a => [a] -> [a] függvényt, mely elhagyja az
ismétléseket! -}
uniq :: Ord a => [a] -> [a]
uniq xs = map (\(x:xs) -> x) (group (sort xs))

{- 5. Definiáld a repeated függvényt, mely csak az ismétlődő elemeket tartja meg egy
listában! -}
repeated :: Ord a => [a] -> [a]
repeated xs = map (\(x:xs) -> x) (filter (\ys -> length ys /= 1) (group (sort xs)))

{- 6. Definiáld újra a zipWith függvényt, mely hasonló a ziphez, azonban nem párokat
készít, hanem egy kétparaméteres függvényt alkalmaz! -}
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = ((f x y):(zipWith' f xs ys))

{- 7. Definiáld két vektor skaláris szorzatát! Használd a zipWith-et. -}
dotProduct :: (Num a) => [a] -> [a] -> a
dotProduct v1 v2 = sum (zipWith (*) v1 v2)

{- 8. Definiáld az isPrime függvényt, mely eldönti, hogy egy egészszám prím-e! -}
isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime i = not (any (\x -> i `mod` x == 0) [2..i - 1])

{- 9. Definiáld a primes végtelen listát! -}
primes :: [Integer]
primes = [ p | p <- [1..], isPrime p ]

{- 10. *Definiáld az iterate :: (a -> a) -> a -> [a] függvényt, mely egy végtelen
listát épít fel! -}
iterate' :: (a -> a) -> a -> [a]
iterate' f start = start:(iterate f (f start))

{- 11. *Definiáld a fibonacci végtelen listát a fenti iteratesegítségével! -}
fibonacci :: [Integer]
fibonacci = map fst (iterate' (\(a, b) -> (b, a + b)) (0, 1))
