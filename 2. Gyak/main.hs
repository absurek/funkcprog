type Book = (String, String, Int, Bool)

bevanal :: Book
bevanal = ("Bevanal", "Simon Petya", 2010, True)

hp :: Book
hp = ("HP", "J. K. Rowling", 1998, False)

thinkCpp :: Book
thinkCpp = ("Thinking in Cpp", "Bruce Eckel", 2000, False)

dataBase :: [Book]
dataBase = [bevanal, hp, thinkCpp]

getTitle :: Book -> String
getTitle (title, _, _, _) = title

isAvailable :: Book -> Bool
isAvailable (_, _, _, available) = available

availableBooks :: [Book] -> [Book]
availableBooks dataBase = [book | book <- dataBase, isAvailable book]