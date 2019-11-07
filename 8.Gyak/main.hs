{- 1. Definiáld újra a take függvényt! -}
take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (first:rest)
    | n == 0 = []
    | n > 0  = (first:(take' (n - 1) rest))

{- 2. Definiáld újra adropfüggvényt! -}
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (first:rest)
    | n == 0 = (first:rest)
    | n > 0 = drop' (n - 1) rest

{- 3. Adott egy nyelvazonosító (például hu-HU vagy en-US), mely kétrészből, nyelvből és
régióból tevődik össze. Bonts fel egy nyelvazonosítót nyelvre és régióra! Feltesszük,
hogy jól formázott a bemenet. Mindkét rész két-két karakterből áll. -}
langAndRegion :: String -> (String, String)
langAndRegion str = (take' 2 str, drop' 3 str)

{- 4. Definiáld újra a zip függvényt! -}
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = ((x, y):(zip' xs ys))

{- 5. Definiáld újra az unzip függvényt! -}
unzip' :: [(a, b)] -> ([a], [b])
unzip' ((x, y):[]) = (x:[], y:[])
unzip' ((x,y):rest) = ((x:(fst unzipRest)), (y:(snd unzipRest)))
    where unzipRest = unzip' rest

{- 6. Adott egy szöveges fájl. Hanyadik sorai üresek? A sorszámozás 1-től induljon! Az
utolsó sor nem számít üresnek. Sorok feldarabolására a lines használható. -}
empty :: String -> [Int]
empty str = [ index | (index, line) <- (zip' [1..] (lines str)), line == "" ]

{- 7. Bonts fel egy listát két részre egy adott pozíción! -}
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' index list = 
    ([ val | (i, val) <- (zip' [0..] list), i < index ], 
     [val | (i, val) <- (zip' [0..] list), i >= index])

{- 8. Definiáld újra a nub függvényt! Ez minden elemet csak egyszer tart meg, annak is
csak a legelső előfordulását. -}
nub':: Eq a => [a] -> [a]
nub' list = nub'' list []
  where
    nub'' [] _ = []
    nub'' (first:rest) foundElemsList
      | first `elem` foundElemsList = nub'' rest foundElemsList
      | otherwise = (first:(nub'' rest (first:foundElemsList)))

{- 9. Definiáld újra a concat függvényt! -}
concat' :: [[a]] -> [a]
concat' lists = [ e | l <- lists, e <- l ]
