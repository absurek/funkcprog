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
users = [ ("mrbean", "4321"), ("admin", "s3cr3t"), ("finn", "algebraic")]

doesUserExist :: String -> [(String, String)] -> Bool
doesUserExist usrnm usrs = any (\u -> fst u == usrnm) usrs

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

{- 12. Gyakorlat -}

{- 1. Definiálj egy ú.n. smart constructort időpontokhoz! Ez Maybe-t használ a
sikertelenség jelzésére (error helyett). -}
type Hour   = Int
type Minute = Int
data T = T Hour Minute deriving (Show, Eq)

time h m | h < 0 || h >= 24  || m < 0 || m >= 60 = Nothing
         | otherwise                             = Just (T h m)

eqTime :: T -> T -> Bool
eqTime a b = a == b

{- 2.Definiáld egy változatát az időpontnak, mely 12-órás am pm formában tárolja az
időpontokat! Legyen ez USTime! Kérj hozzá egyenlőségvizsgálatot és szöveggé alakítást (Eq és Show)! -}
--data AmPm = AM | PM deriving (Show, Eq)
data USTime = AM Hour Minute | PM Hour Minute deriving (Show, Eq)

{- 3. Definiáld, hogyan lehet egy USTime időpontot szöveggé alakítani! -}
showUSTime :: USTime -> String
showUSTime (AM h m) = show h ++ "." ++ show m ++ " am"
showUSTime (PM h m) = show h ++ "." ++ show m ++ " pm"

{- 4. Alakíts át egy USTime időpontot Time-ra! 12.00 am éjfélt (00.00), 12.00 pm delet
jelent (a 12 óra a 12-órás időben 0 órának „felel meg”). -}
usTimeToTime :: USTime -> T
usTimeToTime (AM h m) = if h == 12 then (T 0 m) else (T h m)
usTimeToTime (PM h m) = if h == 12 then (T h m) else (T (h + 12) m)

{- 5. Alakíts vissza egy Timeidőpontot USTime-ra! -}
timeToUSTime :: T -> USTime
timeToUSTime (T h m) | h == 12 = (PM h m)
                     | h == 0  = (AM 12 m)
                     | h < 12  = (AM h m)
                     | h > 12  = (PM (h - 12) m)

{- 13. Gyakorlat -}

{- 1. Definiálj jogosultsági szinteket (Privilege) egy webalkalmazáshoz! Két szint a
felhasználó (Unprivileged) és rendszergazda (Admin). A jogosultsági szinteket ábrázold
egy új (felsorolási) típussal! -}
data Privilege = Unprivileged | Admin deriving (Show, Eq)

{- 2. Definiálj egy Cookie adattípust a webalkalmazáshoz, mellyel a webalkalmazás
adatokat tárol a felhasználók böngészőjében! A Cookie-ben egy felhasználó állapotát
tartjuk számon. Egy felhasználó bejelentkezett adott felhasználónévvel és jogosultsági
szinttel vagy kijelentkezett. -}
type UserName = String
type Password = String
data Cookie = LoggedIn UserName Privilege | LoggedOut deriving (Show, Eq)

{- 3. A webalkalmazás adatbázisa felhasználók adataiból (felhasználónévből, jelszóból,
jogosultsági szintből) áll. Az adatbázist az adatok listájával modellezzük. -}
type UserDatabase = [(UserName, Password, Privilege)]
db :: UserDatabase
db = [("dumbledore","abracadabra",Unprivileged), ("root", "secret", Admin),
      ("bela", "korte", Unprivileged)]

{- 4. Definiálj egy függvényt új felhasználók felvételére! Ehhez csak rendszergazdának
van jogosultsága. Egyéb felhasználók esetén az adatbázis ne változzon! -}
register :: UserName -> Password -> Cookie -> UserDatabase -> UserDatabase
register _ _ LoggedOut database = database
register userName pwd (LoggedIn _ priv) database
    | priv /= Admin = database
    | otherwise     = (userName, pwd, Unprivileged):database

{- 5. Definiálj egy függvényt, mely lekéri az adatbázisból egy felhasználó jelszavát
és jogosultsági szintjét! Használd a Maybe típust! -}
getUser :: UserName -> UserDatabase -> Maybe (Password, Privilege)
getUser _ [] = Nothing
getUser userName ((name, pwd, priv):xs)
    = if userName == name then Just (pwd, priv) else getUser userName xs

{- 6. Definiálj egy függvényt, mely leellenőrzi egy felhasználó nevét és jelszavát!
Helyes adatok esetén egy bejelentkezett Cookiet, egyébként kijelentkezett Cookiet
adjon vissza! -}
login :: UserName -> Password -> UserDatabase -> Cookie
login userName pwd database | user == Nothing = LoggedOut
                            | pwd == p  = (LoggedIn  userName pr)
                            | otherwise = LoggedOut
                              where (Just (p, pr)) = user
                                    user = (getUser userName database)

{- 7. Írj egy jelszócserélő függvényt! A jelszót csak egy bejelentkezett felhasználó
változtathassa meg, mindenki csak a sajátját. Egy kijelentkezett felhasználó esetén az
adatbázis ne változzon! -}
passwd :: Password -> Cookie -> UserDatabase -> UserDatabase
passwd _ LoggedOut database = database
passwd _ _ [] = []
passwd pwd (LoggedIn userName priv) ((name, p, pr):xs) =
    if userName == name then (name, pwd, pr):xs
    else (name, p, pr):(passwd pwd (LoggedIn userName priv) xs)

{- 8. Definiálj egy függvényt egy felhasználó törlésére! Ehhez csak rendszergazdának
van jogosultsága. Egyéb felhasználók esetén az adatbázis ne változzon! -}
delete :: UserName -> Cookie -> UserDatabase -> UserDatabase
delete _ _ [] = []
delete userName (LoggedIn adminName Admin) ((name, p, pr):xs) =
    if name == userName then xs
    else (name, p, pr):(delete userName (LoggedIn adminName Admin) xs)
delete _ _ database = database

{- 9. Listázd ki a felhasználókat az adatbázisban! -}
users :: UserDatabase -> [UserName]
users database = [ name | (name, _, _) <- database ]
