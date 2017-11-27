import Data.Matrix
import System.Random


data Weapon = Rock | Paper | Scissor deriving (Show, Eq)
type Prob = [(Double, Weapon)]

mat = [Rock, Paper, Scissor, Rock, Rock, Paper, Rock, Rock, Scissor]
list = [(33.33, Rock), (33.33, Paper), (33.33, Scissor)]

matB = fromList 3 3 mat

main :: IO ()
main = do
    gen <- getStdGen

    putStrLn $ show $ matB
    putStrLn $ show $ extendField matB list gen


extendField :: Matrix Weapon -> Prob -> StdGen -> [[Weapon]]
extendField field normalProbList seed =  newMatrix
    where fieldLists = toLists field
          (newLine,newSeed) = generate (length fieldLists) ([], seed)
          newMatrix = fieldLists ++ [newLine]
          generate 0 (list, seed) = (list, seed)
          generate counter (list, seed) -- TODO: also check left if i > 0
            | (fieldLists!!(length fieldLists-1)!!(length fieldLists - counter)) == Rock = generate (counter-1) (list ++ [ Rock ], (seed))
            | otherwise = generate (counter-1) (list ++ [ fst $ randomChooseTile normalProbList seed ], (snd $ randomChooseTile normalProbList seed))



randomChooseTile :: Prob -> StdGen -> (Weapon, StdGen)
randomChooseTile probList seed = (generateTile probList rand, newSeed)
  where (rand, newSeed) = randomR (0,100) seed :: (Double, StdGen)
        generateTile ((probTile, tile):xs) rand
          | rand <= probTile = tile
          | otherwise = generateTile xs (rand - probTile)
