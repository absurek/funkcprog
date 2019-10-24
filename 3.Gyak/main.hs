{- ELTE-IK FunckProg, 3. gyakorlat, Függvények definiálása több egyenlettel,
Mintaillesztés alap típusokra -}

{- 1. Definiáld újra a logikai „és” (&&) operátort mintaillesztés-sel! -}
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

{- 2. Definiáld  újra  a  logikai  „vagy”  (||)  operátortmintaillesztéssel! -}
or' True _ = True
or' _ True = True
or' _ _ = False

{- 3. Definiáld újra a logikai „kizáró vagy” (xor) operátortmintaillesztéssel! -}
xor True False = True
xor False True = True
xor _ _ = False

{- 4. Definiáld a kettes számrendszerbeli összeadástmintaillesztéssel! Add vissza az
összeget és az átvitelt is! -}
add2 :: Int -> Int -> (Int, Int)
add2 0 0 = (0, 0)
add2 0 1 = (1, 0)
add2 1 0 = (1, 0)
add2 1 1 = (0, 1)
add2 _ _ = error "add2 called with non-binary arguments"

{- 5. Definiálj egy függvényt, mely megvizsgálja, hogy két záró-jel összeillik-e! -}
paren :: Char -> Char -> Bool
paren '(' ')' = True
paren '{' '}' = True
paren '[' ']' = True
paren _ _ = False

{- 6. Készíts egy egyszerű számológépet, mely képes a négy alap-művelet elvégzésére!
Használj mintaillesztést a műveletvizsgálatára! -}
calc :: (Int, Char, Int) -> Int
calc (a, '+', b) = a + b
calc (a, '-', b) = a - b
calc (a, '*', b) = a * b
calc (a, '/', b) = a `quot` b
calc (_, _, _) = error "Invalid arguments for calc"

{- 7. Vizsgáld meg mintaillesztéssel, hogy a bemenő paraméterszóköz-e! -}
isSpace :: Char -> Bool
isSpace ' ' = True
isSpace _ = False
