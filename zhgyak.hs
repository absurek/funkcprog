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

-- 3. Zh feladatsor --
import Data.Char
import Data.List

{- 1. Melyik listában van az elem? (1 pont)
Adott három lista és egy elem. Döntsd el, melyikben található a keresett elem! Ha egyikben sem, az eredmény 0. -}
which :: ([Char], [Char], [Char]) -> Char -> Int
which (a, b, c) e
    | e `elem` a = 1
    | e `elem` b = 2
    | e `elem` c = 3
    | otherwise  = 0

{- 2. Dominók illeszthetősége (1 pont)
Állapítsd meg, hogy két dominólap illeszthető-e egymáshoz! Ez akkor áll fenn, ha a
dominók két oldala közül van, amelyiken azonos számú pont van. Egy dominót a két
oldalán lévő pontok számával jelölünk. Elég az első dominó jobb oldalát és a második
bal oldalát összevetni. -}
matches :: (Int, Int) -> (Int, Int) -> Bool
matches (_, a) (b, _) = if a == b then True else False

{- 3. Nagybetűsítés (1 pont)
Alakítsuk nagybetűvé egy szöveg első betűjét! Ha az első karakter nem betű, akkor
hagyjuk a szöveget változatlanul! -}
toUpperCase :: String -> String
toUpperCase [] = []
toUpperCase (x:xs) = (toUpper x):xs

{- 4. Maybe-csere (1 pont)
Cserélj ki egy Maybe belsejében lévő értéket egy másikra! A Nothing-ból Nothing lesz. -}
swap :: Maybe a -> b -> Maybe b
swap Nothing _ = Nothing
swap (Just a) b = Just b

{- 5. Fájljogosultságok (2 pont)
Unix rendszerekben a fájlok engedélyeit szokás szimbolikusan (pl. "rwx") és számszerűen
(pl. 7) is jelölni.

Valósítsd meg a szimbolikus jelölésről számszerűre konverziót!

Feltesszük, hogy a bemenetben csak 'r', 'w' és 'x' betűk szerepelnek, mindegyik csak
legfeljebb egyszer.

A betűk tetszőleges sorrendben lehetnek. -}
numeric :: String -> Int
numeric [] = 0
numeric (x:xs) = letterToInt x + numeric xs
    where letterToInt 'x' = 1
          letterToInt 'w' = 2
          letterToInt 'r' = 4
          letterToInt _   = 0

{- 6. Pitagoraszi számhármasok (2 pont)
Gyűjtsd egy listába a pitagoraszi számhármasokat! Ezek azok az a, b, c
számhármasok, melyekre teljesül a^2 + b^2 == c^2.
Elegendő csak 1 <= a, b, c <= 100 tartományban keresni.
Az ismétléseket kerüljük: (3, 4, 5) és (4, 3, 5) közül csak az egyik szerepeljen.
Megjegyzés: a tesztesetben használt sort függvény a Data.List modulból érhető el. -}
pythagoreans :: [(Int, Int, Int)]
pythagoreans = [ (a, b, c) | a <- [1..100], b <- [1..100], c <- [1..100],
                 a^2 + b^2 == c^2, a <= b && b < c ]

{- 7. Van-e hosszú szó? (2 pont)
Vizsgáld meg, van-e legalább n > 0 betűből álló szó egy szövegben! -}
hasLongWord :: Int -> String -> Bool
hasLongWord n str = hasLongWord' n (words str)
    where hasLongWord' _ []     = False
          hasLongWord' n (x:xs) = if length x >= n then True else hasLongWord' n xs

{- 8. Minimális szélesség (2 pont)
Egészíts ki egy szöveget szóközökkel balról megadott hosszúságra! Ha a szöveg eleve
hosszabb volt a meghatározottnál, ne vágj le belőle! -}
align :: Int -> String -> String
align n str = if length str < n then align n (' ':str) else str

{- 9. Fejelem módosítása (2 pont)
Egy lista első elemét módosítsd egy Maybe-t adó f függvénnyel! Ha f egy Nothing-ot ad,
töröld a lista első elemét! Ha Just x-et, akkor cseréld le az első elemet x-re! -}
modify :: (a -> Maybe a) -> [a] -> [a]
modify _ []     = []
modify f (x:xs) = if validX xRes then (getNewX xRes):xs else xs
    where xRes = f x
          validX Nothing = False
          validX _       = True
          getNewX (Just a) = a

{- 10. Hosszabb-e a lista, mint n? (2 pont)
Írd meg a length egy olyan változatát, mely megvizsgálja, hogy hosszabb-e egy lista,
mint egy előre megadott n >= 0 méret! -}
isLonger :: [a] -> Int -> Bool
isLonger [] _     = False
isLonger _ 0      = True
isLonger (x:xs) n = isLonger xs (n - 1)

{- 11. Ékezetes betűk cseréje (3 pont)
Cseréld ki az ékezetes betűket az ékezet nélküli párjukra! Elegendő csak a kisbetűkkel
foglalkozni.-}
removeAccents :: String -> String
removeAccents [] = []
removeAccents (x:xs) = (removeAccent x charPairs):(removeAccents xs)
    where accentedChars    = ['á', 'é', 'í', 'ö', 'ő', 'ú', 'ü', 'ű']
          nonAccentedChars = ['a', 'e', 'i', 'o', 'o', 'u', 'u', 'u']
          charPairs = zip accentedChars nonAccentedChars
          removeAccent c []     = c
          removeAccent c (x:xs) = if c == fst x then snd x else removeAccent c xs

{- 12. Aláhúzásjelek levágása (2 pont)
Távolítsd el az aláhúzásjeleket egy szöveg elejéről és végéről! -}
strip :: String -> String
strip []  = []
strip str = stripBack (stripFront str)
  where stripFront ('_':xs) = stripFront xs
        stripFront str      = str
        stripBack str = reverse (stripFront (reverse str))

{- 13. Kő-papír-olló (2 pont)
Definiálj egy RPS adatszerkezetet a kő-papír-olló játékhoz! Három lehetőség a Rock,
Paper, Scissors. Kérj legalább egyenlőségvizsgálatot is (deriving (Eq))! -}
data RPS = Rock | Scissors | Paper deriving (Eq, Show, Enum)

{- Definiálj egy függvényt, mely megmondja, melyik jel melyiket üti! -}
beats :: RPS -> RPS
beats Paper = Rock
beats a = succ a

{- 14. Kő-papír-olló játszma (2 pont)
Két játékos kő-papír-ollót játszik. Felírják, hogy ki milyen jelet mutatott. Segíts
nekik megszámolni, hogy az első játékos hányszor nyert!

Feltesszük, hogy mindkét lista azonosan hosszú. -}
firstBeats :: [RPS] -> [RPS] -> Int
firstBeats fPlays sPlays = countFBeats 0 (zip fPlays sPlays)
    where countFBeats n [] = n
          countFBeats n (x:xs) = if beats (fst x) == (snd x)
                                 then countFBeats (n + 1) xs
                                 else countFBeats n xs

{- 15. Hőmérséklet mérése (2 pont)

Definiálj egy Temperature adatszerkezetet a levegőhőmérséklet mérésekhez! A hőmérsékletet mérjük nappal és éjszaka (Daytime és Night). -}
data Temperature = Daytime Int | Night Int deriving (Show) 

{- Döntsd el egy mérésről, hogy nappal történt-e vagy éjszaka! -}
isDaytime :: Temperature -> Bool
isDaytime (Daytime _) = True
isDaytime _           = False

{- 6. Szélsőséges hőmérsékletek (3 pont)
Adott egynapi négyóránkénti méréssorozat. Állapítsd meg a legmagasabb nappali, és legalacsonyabb éjszakai hőmérsékletet!-}
extremes :: [Temperature] -> (Int, Int)
extremes l = (maximum (map (\(Daytime t) -> t) dayTimeTemps),
              minimum (map (\(Night t) -> t) nightTimeTemps))
    where dayTimeTemps = filter (\x -> isDaytime x) l
          nightTimeTemps = filter (\x -> not (isDaytime x)) l
