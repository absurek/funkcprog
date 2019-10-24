{- ELTE-IK FunckProg, 7. gyakorlat, Rekurzió listán -}

{- 1. Definiáld újra a sum függvényt! -}
sum' :: [Int] -> Int
sum' [] = 0
sum' (first:rest) = first + sum' rest

{- 2. Definiáld újra a last függvényt! A last üres listán nincs értelmezve. -}
last' :: [Int] -> Int
last' [] = error "last' called with empty list"
last' (first:[]) = first
last' (first:rest) = last' rest

{- 3. Definiáld újra az and függvényt! -}
and' :: [Bool] -> Bool
and' [] = True
and' (first:rest) = first && and' rest

{- 4. Definiáld újra az or függvényt! -}
or' :: [Bool] -> Bool
or' [] = False
or' (first:rest) = first || or' rest

{- 5. Definiáld újra a replicate függvényt! -}
replicate' :: Int -> Char -> String
replicate' 0 _ = ""
replicate' n c = (c:(replicate' (n - 1) c))

{- 6. Egészíts ki egy szöveget megadott szélességűre! Ha netán a megadott szélesség
kisebb lenne, mint a szöveg, akkor ne vágj lebelőle! -}
format :: Int -> String -> String
format n s
    | length s < n - 1 = (' ':(format (n - 1) s))
    | otherwise = s

{- 7. Szúrj be egy egész számot nagyság szerinti helyére! -}
insert :: Int -> [Int] -> [Int]
insert e [] = (e:[])
insert e (first:rest)
    | e <= first = (e:first:rest)
    | otherwise = (first:(insert e rest))

{- 8. Valósítsd meg a beszúró rendezést! Ez egy üres listába szúrja be egyesével a
rendezetlen elemeket nagyság szerint megfelelőhelyre. -}
sort :: [Int] -> [Int]
sort [] = []
sort (first:rest) = insert first (sort rest)

{- 9. *Fésülj össze két rendezett, egészekből álló listát úgy, hogy az eredmény is
rendezett maradjon! -}


{- 10. *Valósítsd meg az összefésüléses (merge sort) rendezést, mely egy listát
kettébont középen, rendezi a két félt, majdösszefuttatja a kettőt! -}

{- 11. Definiálj egy breakOn függvényt, mely egy szövegben egy megadott karakter
előtti és utáni darabot visszaadja! -}
breakOn :: Char -> String -> (String, String)
breakOn _ [] = ([], [])
breakOn delim (x:xs)
    | x == delim = ([], (x:xs))
    | otherwise = ((x:(fst (breakOnRest))), snd breakOnRest)
  where breakOnRest = breakOn delim xs

{- 12. Definiálj egy splitOn függvényt, mely adott karakter mentén darabol egy
Stringet! -}
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim (first:rest)
    | delim == first = splitOn delim rest
    | otherwise = ((fst breakOnRest):(splitOn delim (snd breakOnRest)))
  where breakOnRest = breakOn delim (first:rest)

{- 13. Bonts fel cellákra egy csv fájl tartalmát! A fájl sorokra való bontásához
érdemes használni a lines függvényt. -}
csv :: String -> [[String]]
csv str = [ splitOn ',' line | line <- lines str ]
