import Data.Matrix
import System.Random


data Weapon = Rock | Paper | Scissor deriving (Show, Eq)
type Prob = [(Double, Weapon)]

mat = [Rock, Paper, Paper, Rock, Rock, Paper, Scissor, Paper, Paper]
list = [(33.33, Rock), (33.33, Paper), (33.33, Scissor)]

matB = fromList 3 3 mat

main :: IO ()
main = do
    gen <- getStdGen

    putStrLn $ show $ matB
    putStrLn $ show $ extendField matB list gen
    --putStrLn $ show $ extendFieldBis matB list gen
    putStrLn $ show $ fromLists $ extendField matB list gen


-- TODO: last line need one tile more
extendField :: Matrix Weapon -> Prob -> StdGen -> [[Weapon]]
extendField field normalProbList seed =  newMatrix
    where fieldLists = extendFieldBis field normalProbList seed
          (newLine,newSeed) = generate (length fieldLists) ([], seed)
          newMatrix = fieldLists ++ [newLine]
          generate (-1) (list, seed) = (list, seed)
          generate counter (list, seed)
            | counter /= (length fieldLists) && (list!!(length fieldLists - (counter + 1))) == Rock = generate (counter-1) (list ++ [ Rock ], (seed))
            | (fieldLists!!(length fieldLists-1)!!(length fieldLists - counter)) == Rock = generate (counter-1) (list ++ [ Rock ], (seed))
            | otherwise = generate (counter-1) (list ++ [ fst $ randomChooseTile normalProbList seed ], (snd $ randomChooseTile normalProbList seed))

-- TODO: upper check
extendFieldBis :: Matrix Weapon -> Prob -> StdGen -> [[Weapon]]
extendFieldBis field normalProbList seed = newMatrix
    where fieldLists = toLists field
          (newMatrix, newSeed) = extendLoop (length fieldLists) ([], seed)
          extendLoop 0 (list, seed) = (list, seed)
          extendLoop counter (list, seed)
            | (fieldLists!!(length fieldLists - counter)!!(length fieldLists - 1)) == Rock = extendLoop (counter-1) (list ++ [(fieldLists!!(length fieldLists - counter)) ++ [ Rock ]], seed)
            | otherwise = extendLoop (counter-1) (list ++ [(fieldLists!!(length fieldLists - counter)) ++ [ fst $ randomChooseTile normalProbList seed ]], snd $ randomChooseTile normalProbList seed)



randomChooseTile :: Prob -> StdGen -> (Weapon, StdGen)
randomChooseTile probList seed = (generateTile probList rand, newSeed)
  where (rand, newSeed) = randomR (0,100) seed :: (Double, StdGen)
        generateTile ((probTile, tile):xs) rand
          | rand <= probTile = tile
          | otherwise = generateTile xs (rand - probTile)
