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
