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
