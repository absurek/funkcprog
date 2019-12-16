-- 1. Zh feladatsor --
{- 1. Kettő hatványai (1 pont)
Definiáljuk az f1 függvényt, amely kiszámolja az n.n. kettőhatványt (ahol nn nemnegatív
szám)! -}

f1 :: Int -> Int
f1 n = 2 ^ n

{- 2. Logikai értékek (1 pont)
Definiáljuk az f2 függvényt, amely akkor ad vissza True értéket, ha pontosan két paramétere True értékű! -}
f2 :: Bool -> Bool -> Bool -> Bool
f2 True True False = True
f2 True False True = True
f2 False True True = True 
f2 _ _ _           = False

{- 3. Rendezett hármas fordítása (1 pont)
Definiáljuk az f3 függvényt, amely egy hármas (3 elemű tuple) elemeinek sorrendjét
megfordítja! -}
f3 :: (a, b, c) -> (c, b, a)
f3 (x, y, z) = (z, y, x)

{- 4. Első három elem összege (1 pont)
Definiáljuk az f4 függvényt, amely egy lista első 3 elemének összegét adja meg! -}
f4 :: [Int] -> Int
f4 l = sum (take 3 l)

{- 5. Rendezettség (2 pont)
Definiáljuk azt a függvényt, amely eldönti egy listáról, hogy az rendezett-e! Egy
listát rendezettnek tekintünk, ha az elemei növekvő sorrendben vannak. -}
f5 :: Ord a => [a] -> Bool
f5 []       = True
f5 [x]      = True
f5 (x:y:xs) = if x <= y then f5 (y:xs) else False

{- 6. Utolsó két elem (2 pont)
Definiáljuk az f6 függvényt, amely a lista utolsó előtti elemét adja vissza! -}
f6 :: [a] -> a
f6 l = last (init l)

{- 7. Leghosszabb szó (2 pont)
Keressük meg a leghosszabb szót egy szövegben! Feltesszük, hogy van legalább egy szó a
szövegben. -}
f7 :: String -> String
f7 str = maxLength (words str)
    where maxLength [x] = x
          maxLength (x:y:xs) = if length x < length y then maxLength (y:xs)
                               else maxLength (x:xs)

{- 8. Elem eltávolítása (3 pont)
Definiáljuk az f8 függvényt, amely egy adott elemet keres a listában és eltávolítja az
azt követő elemet (csak a legelső alkalommal)! -}
f8 :: Eq a => a -> [a] -> [a]
f8 _ [] = []
f8 _ [x] = [x]
f8 e (x:y:xs) = if e == x then x:xs else (x:y:(f8 e xs))

{- 9. Függvénykompozíció (3 pont)
Készíts egy nagy függvénykompozíciót függvények listájából!

Megjegyzés: A listában lévő utolsó függvényt alkalmazzuk először, tehát jobbról balra
haladunk. -}
f9 :: [(a -> a)] -> (a -> a)
f9 [f] = f
f9 l = foldr (.) (\x -> x) l

{- 10. Feltételes függvény (2 pont)
Definiáljuk az f10 függvényt, amely 3 paramétert kap! Ha az első paraméter True, akkor
alkalmazza a második paraméterként átadott függvényt a harmadik paraméterre. Egyébként
változatlanul visszaadja a harmadik paramétert. -}
f10 :: Bool -> (a -> a) -> a -> a
f10 True f x = f x
f10 _ _ x    = x

{- 11. Feltételes listatranszformáció (3 pont)
Definiáljuk az f11 függvényt, amely 3 paramétert kap. Az első paraméter egy logikai
értékű függvény, amely megadja, hogy hol kell alkalmazni a második függvényt. A
harmadik paraméter a lista, amelynek az elemein a változtatás megtörténik. -}
f11 :: (a -> Bool) -> (a -> a) -> [a] -> [a]
f11 ff mf l = [ if ff x then mf x else x | x <- l ]

{- 12. Legkisebb elem (2 pont)
Definiáljuk az f12 függvényt, amely egy lista legkisebb elemét adja vissza egy Maybe
értékben. Ha a lista üres, akkor Nothing értéket ad vissza. -}
f12 :: Ord a => [a] -> Maybe a
f12 [] = Nothing
f12 l = Just (minimum l)

{- 13. Hétköznapok (2 pont)
Hozz létre egy Day adattípust, mely a hét napjait ábrázolja Mon, Tue, Wed, Thu, Fri,
Sat, Sun adatkonstruktorokkal. Írj deriving (Eq) záradékot hozzá. -}
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq)

{-Írj egy f13 függvényt, mely eldönti, hogy egy nap hétköznap-e! -}
f13 :: Day -> Bool
f13 d = if d == Sat || d == Sun then False
                                else True

{- 14. Következő munkanap (1 pont)
Írj egy f14 függvényt, amely megadja a következő munkanapot! -}
f14 :: Day -> Day
f14 Mon = Tue
f14 Tue = Wed
f14 Wed = Thu
f14 Thu = Fri
f14 _   = Mon

{- 15. Kezdőbetűk (2 pont)
Egy névnek adjuk meg a kezdőbetűit! -}
f15 :: String -> String
f15 str = unwords [ x:[] | x:xs <- words str ]

{- 16. Négyzetszámok (2 pont)
Adjuk meg azt a függvényt, amely eldönti egy számról, hogy az négyzetszám-e! -}
f16 :: Int -> Bool
f16 n = n `elem` [ x * x | x <- [0..(n + 1) `div` 2]]

-- 2. Zh feladatsor --

{- 1. modDiv (1 pont)
Add meg két természetes szám egymással vett egész osztásának maradékát, és annak
eredményét is egy rendezett párban! A pár első komponense legyen a maradék, a második
pedig az eredmény. -}
modDiv :: Int -> Int -> (Int,Int)
modDiv a b = (a `mod` b, a `div` b)

{- 2. swapFirstTwo (1 pont)
Cseréld meg egy lista első két elemét! Amennyibben a listának nincs legalább két eleme,
úgy hagyd változatlanul! -}
swapFirstTwo :: [a] -> [a]
swapFirstTwo (x:y:xs) = y:x:xs
swapFirstTwo l = l

{- 3. swapFirstTwos (2 pont)
Cseréld meg egy listában lévő összes listának az első két elemét! -}
swapFirstTwos :: [[a]] -> [[a]]
swapFirstTwos [] = []
swapFirstTwos (x:xs) = (swapFirstTwo x):(swapFirstTwos xs)

{- 4. isPalindrome (2 pont)
Döntsd egy listáról, hogy palindrom-e! Egy lista pontosan akkor palindrom, ha
megegyezik a megfordítottjával. -}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == reverse l

{- 5. palindromize (2 pont)
Alakíts palindrommá egy listát! Ehhez fűzd össze a megfordítottjával, de olyan módon,
hogy a "középső" (azaz az eredeti lista utolsó) eleme ne legyen megduplázva (lásd
tesztek). -}
palindromize :: [a] -> [a]
palindromize [] = []
palindromize l = l ++ reverse' l
    where reverse' [x] = []
          reverse' (x:xs) = reverse' xs ++ [x]

{- 6. oddPlusOne (2 pont)
Definiáljuk az oddPlusOne függvényt, amely egy számokból álló lista összes páros elemét
eltávolítja, a maradékhoz pedig hozzáad 1-et! -}
oddPlusOne :: [Int] -> [Int]
oddPlusOne l = map (+1) (filter odd l)

{- 7. deleteAll (2 pont)
Töröld egy listából egy adott elem összes előfordulását! -}
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll e l = filter (\x -> x /= e) l

{- 8. countEmpties (2 pont)
Számold meg, hogy hány darab üres lista van egy listában! -}
countEmpties :: [[a]] -> Int
countEmpties l = countEmtyLists 0 l
    where countEmtyLists cnt [] = cnt
          countEmtyLists cnt (x:xs) = if null x then countEmtyLists (cnt + 1) xs
                                                else countEmtyLists cnt xs

{- 9. onlyVowels (2 pont)
Válogasd ki egy String-ből az angol abécé magánhangzóit (e,u,i,o,a)!
Megjegyzés: Feltételezhetjük, hogy a kapott String-ben kizárólag
kisbetűk szerepelnek. -}
onlyVowels :: String -> String
onlyVowels str = filter (\x -> isVowel x) str
    where isVowel c = any (\x -> x == c) ['e','u','i','o','a']

{- 10. heads (3 pont)
Vegyük listában lévő listáknak az első elemeit! Ha egy belső lista üres, akkor ugord
át! -}
heads :: [[a]] -> [a]
heads [] = []
heads (x:xs) = if null x then heads xs else (head x):(heads xs)

{- 11. conditionalApply (2 pont)
Valósítsd meg a feltételes applikációt! Az első paramétere egy predikátum (logikai
értékű függvény), a második pedig az alkalmazandó függvény. Amennyiben a kapott elemre
igaz a predikátum, akkor alkalmazza rá a függvényt, egyébént adja vissza
változatlanul. -}
conditionalApply :: (a -> Bool) -> (a -> a) -> a -> a
conditionalApply pr f e = if pr e then f e else e

{- 12. conditionalMap (3 pont)
Valósítsd meg conditionalMap függvényt! Ez a függvény nagyon hasonló a már megszokott
map függvényhez. Annyiban különbözik tőle, hogy kap egy extra paramétert, egy feltétel
függvényt. Pontosan akkor fogja alkalmazni a második paraméterként kapott függvényt a
kapott értékre, ha arra a feltétel függvény igazat ad vissza (egyébként változatlanul
hagyja). -}
conditionalMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
conditionalMap pr f l = map (conditionalApply pr f) l

{- 13. isWeekend (2 pont)
Hozz létre egy Day adattípust, mely a hét napjait ábrázolja Mon, Tue, Wed, Thu, Fri,
Sat, Sun adatkonstruktorokkal! Írj deriving (Eq,Show) záradékot hozzá! -}
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Show)

{- Döntsd egy napról, hogy hétvégére esik-e! -}
isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

{- 14. dayToInt (2 pont)
Alakíts egész számmá egy napot! A hétfőhöz 0-t, a keddhez 1-et, ... a vasárnaphoz pedig
6-ot rendeljen. -}
dayToInt :: Day -> Int
dayToInt d = snd (findDay d dayIntPairs)
    where dayIntPairs = zip [Mon, Tue, Wed, Thu, Fri, Sat, Sun] [0..6]
          findDay d (x:xs) = if fst x == d then x else findDay d xs

{- 15. isEarlier (2 pont)
Döntsd el egy napról, hogy korábban van-e, mint egy másik! -}
isEarlier :: Day -> Day -> Bool
isEarlier d1 d2 = dayToInt d1 < dayToInt d2
