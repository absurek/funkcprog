{- 10. Gyakorlat -}
import Data.Char

{- 1. Definiáld a map függvényt! -}
map' :: (a -> b) -> [a] -> [b]
map' f xs = [ f x | x <- xs ]

{- 2. Definiáld a filter függvényt! -}
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [ x | x <- xs, f x ]

{- 3. Definiálj egy upperToLower nevű függvényt, mely minden nagy-betűt kisbetűvé
alakít, a többit eldobja! -}
upperToLower :: String -> String
upperToLower str = [ toLower c | c <- str, c `elem` ['A'..'Z'] ]

upperToLower' :: String -> String
upperToLower' str = map toLower (filter (`elem` ['A'..'Z']) str)

{- 4. Definiáld az all függvényt! -}
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' f (x:xs) = f x && all' f xs

{- 5. Definiáld az any függvényt! -}
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs) = f x || any' f xs

{- 6. Definiálj egy hasLongLines nevű függvényt, mely megvizsgálja,hogy egy fájl sorai
között van-e legalább 3 szóból álló! -}
hasLongLines :: String -> Bool
hasLongLines str = any' (\ln -> length (words ln) > 3) (lines str)

{- 7. Definiáld újra az elem függvényt, mely megnézi, hogy egy elem része-e egy
listának! Használj magasabb rendű függvényt! -}
elem' :: (Eq a) => a -> [a] -> Bool
elem' e xs = any' (\x -> x == e) xs

{- 8. Definiáld a hasAny függvényt, mely megvizsgálja, hogy egy lista elemei közül
valamelyik előfordul-e egy másik listában! -}
hasAny :: (Eq a) => [a] -> [a] -> Bool
hasAny xs ys = any' (\x -> any' (\y -> y == x) ys) xs

{- 9. Definiáld a takeWhile függvényt! -}
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) = if f x then (x:(takeWhile' f xs)) else []

{- 10. Definiáld a dropWhile függvényt! -}
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs) = if f x then (dropWhile' f xs) else x:xs

{- 11.Definiálj egy dropWord nevű függvényt, mely eldobja az első szót egy szöveg
elejéről! -}
dropWord :: String -> String
dropWord str = dropWhile (\c -> c /= ' ') str

{- 12. Adott egy felhasználónév, illetve egy adatbázis felhasználókat tároló
adattáblája, benne felhasználónévvel és jelszóval. Nézd meg, hogy a felhasználónév
szerepel-e az adattáblában! -}
users :: [(String, String)]

{- 11. Gyak -}
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
fibonacci :: [Float] --TODO
fibonacci = (iterate (\x -> (((1.618034**(x + 1.0)) - (-0.618034**(x + 1.0))) / (sqrt 5))) 1)
users = [ ("mrbean", "4321"), ("admin", "s3cr3t"), ("finn", "algebraic")]

doesUserExist :: String -> [(String, String)] -> Bool
doesUserExist usrnm usrs = any (\u -> fst u == usrnm) usrs
