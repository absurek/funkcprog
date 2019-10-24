{- ELTE-IK FunckProg, 8. gyakorlat, Rekurzió listán, Parametrikusan polimorf
függvények -}

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
hogy jól formázott abemenet. Mindkét rész két-két karakterből áll. -}
langAndRegion :: String -> (String, String)
langAndRegion str = (take' 2 str, drop' 3 str)
