import Control.Monad
import System.Random


data Weapon = Rock | Paper | Scissor deriving (Show)

list = [(33.33, Rock), (33.33, Paper), (33.33, Scissor)]

randomWeapon :: [(Double, Weapon)] -> StdGen -> (Weapon, StdGen)
randomWeapon chanceList seed = (weapChoice, newSeed)
    where (rand, newSeed) = randomR (0,100) seed :: (Double, StdGen)
          weapChoice = choisit chanceList rand
          choisit [(prob, weap)] _ = weap
          choisit ((prob, weap):xs) rand
            | rand <= prob = weap
            | otherwise = choisit xs $ rand - prob

type Prob = [(Double, Weapon)]
randomChoice :: Prob -> StdGen -> (Weapon, StdGen)
randomChoice probList seed = (generateTile probList rand, newSeed)
    where (rand, newSeed) = randomR (0,100) seed :: (Double, StdGen)
          generateTile ((probTile, tile):xs) rand
            | rand <= probTile = tile
            | otherwise = generateTile xs (rand - probTile)

main :: IO ()
main = do
    gen <- getStdGen
    fun gen

fun :: StdGen -> IO()
fun ngen = do
    let (weap, newGen) = randomChoice list ngen
    getLine
    putStrLn $ show weap
    fun newGen

data House = House { nom :: String, age :: Int}

instance Show House where
    show (House a b) = "test: " ++ show a ++ "agé de " ++ show b

mat = [House "un" 1, House "deux" 2, House "trois" 3]
maison = House "maisonTest" 50

test :: House -> House -> House
test h@(House nom age) hb@(House nomB ageB) = House nom (age + ageB)


-- listOfTuple :: [(Int, Char)]
-- listOfTuple = do
-- 	n <- [1,2]
-- 	ch <- ['a', 'b']
-- 	return (n, ch)
--
-- -- guard :: (MonadPlus m) => Bool -> m ()
-- -- guard True = return ()
-- -- guard False = mzero
--
-- type KnightPos = (Int, Int)
--
-- moveKnight :: KnightPos -> [KnightPos]
-- moveKnight (c,r) = do
-- 	(c', r') <-[(c+2, r-1), (c+2,r+1), (c-2, r-1), (c-2,r+1), (c+1,r-2), (c+1,r+2), (c-1,r-2),(c-1,r+2)]
-- 	guard (c' `elem` [1..8] && r' `elem` [1..8])
-- 	return (c', r')
