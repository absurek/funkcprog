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
