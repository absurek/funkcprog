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
