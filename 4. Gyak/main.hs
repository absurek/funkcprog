import Data.Char

headInt :: [Int] -> Int
headInt (n:ns) = n

isSingleton :: [Int] -> Bool
isSingleton []     = False
isSingleton (n:[]) = True
isSingleton _      = False

hasTwoElements :: [Int] -> Bool
hasTwoElements []       = False
hasTwoElements (a:b:[]) = True
hasTwoElements _        = False

toUpperFirst :: String -> String
toUpperFirst [] = []
toUpperFirst (c:rest) = toUpper c : rest

isLetter' :: Char -> Bool
isLetter' a = elem a (['a'..'z'] ++ ['A'..'Z'])

mountain :: Int -> [Int]
mountain i = [1..i-1] ++ [i, i-1..1]