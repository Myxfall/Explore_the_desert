data House = House { nom :: String, age :: Int}

instance Show House where
    show (House a b) = "test: " ++ show a ++ "agé de " ++ show b

mat = [House "un" 1, House "deux" 2, House "trois" 3]
maison = House "maisonTest" 50

test :: House -> House -> House
test h@(House nom age) hb@(House nomB ageB) = House nom (age + ageB)
