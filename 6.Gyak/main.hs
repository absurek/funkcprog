
{- ELTE-IK FunckProg, 6. gyakorlat -}

{- 1. Definiáld a faktoriális függvényt rekurzívan! -}
fact :: Int -> Int
fact 0 = 0
fact 1 = 1
fact n = n * fact (n - 1)

{- 2. Számold ki az n. Fibonacci-számot! -}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

{- 3. * A fib függvény eléggé lassú még kis számokra is. Ennek oka, hogy fib újra és
újra kiszámol bizonyos Fibonacci-számokat.Tudsz találni egy gyorsabb megoldást? -}
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib' :: Int -> Int
fib' n = fibs !! n

{- 4. Definiáld a hatvány függvényt! Haskellben ez aˆoperátor, nevezzük el a
sajátunkat pow-nak. Definiáld rekurzívan, nehasználd aˆ-t! -}
pow :: Int -> Int -> Int
pow n 0 = 1
pow n m = n * pow n (m - 1)

{- 5. Definiálj egy range függvényt, mely felsorolja az egész számokat egy alsó és egy
felső korlát között! Ne használd a [ .. ] kifejezést! Feltesszük, hogy a második
paraméter nem kisebb, mint az első. -}
range :: Int -> Int -> [Int]
range firstElem lastElem
  | firstElem == lastElem = [firstElem]
  | otherwise = (firstElem:(range (firstElem + 1) lastElem))

{- 6. Módosítsd a range függvényt, hogy csökkenő sorozatot is elő tudjon állítani, ha a
második paraméter kisebb, mint az első! -}
range' :: Int -> Int -> [Int]
range' bound1 bound2
  | bound1 == bound2 = [bound1]
  | bound1 <= bound2 = range bound1 bound2
  | otherwise = (bound1:(range' (bound1 - 1) bound2))

{- 7. Definiáld újra a length függvényt, mely megszámolja egy lista elemeit! -}
length' :: [a] -> Int
length' [] = 0
length' [e] = 1
length' (firstElem:rest) = 1 + length' rest

{- 8. Definiáld újra a minimum függvényt, mely megkeresi egy lista legkisebb elemét! -}
minimum' :: (Ord a) => [a] -> a
minimum' [] = error "Bad arg for minimum'"
minimum' [e] = e
minimum' (firstElem:rest)
  | firstElem <= minimum' rest = firstElem
  | otherwise = minimum' rest

{- 9. Definiálj egy függvényt, mely kigyűjti egy lista minden második elemét -}
everySecond :: [a] -> [a]
everySecond [] = []
everySecond [e] = []
everySecond (e1:e2:rest) = (e2:everySecond rest)

{- 10. Definiáld újra az elem függvényt, mely rekurzívan leellenőrzi, hogy egy elem megtalálható-e egy listában -}
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (firstElem:rest)
  | e == firstElem = True
  | otherwise = e `elem'` rest

{- 11.Definiálj egy függvényt, mely kikeresi egy kulcs-érték párokból álló listában egy kulcshoz tartozó értéket! Ha a kulcs nem található, adj hibaüzenetet az error függvénnyel! -}
value :: (Eq keys) => keys -> [(keys, values)] -> values
value _ [] = error "Key not found in value"
value key ((k,v):rest)
  | key == k = v
  | otherwise = value key rest

{- 12. Módosítsd a value függvényt, hogy egy alapértelmezett értéket adjon vissza, ha a keresett kulcs nem található a listában! -}
value' :: (Eq keys) => keys -> values -> [(keys, values)] -> values
value' _ defaultVal [] = defaultVal
value' key defaultVal ((k,v):rest)
  | key == k = v
  | otherwise = value' key defaultVal rest
