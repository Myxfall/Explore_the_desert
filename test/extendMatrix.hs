import Data.Matrix
import System.Random


data Weapon = Rock | Paper | Scissor deriving (Show, Eq)
type Prob = [(Double, Weapon)]
type Position = (Int, Int)

pos = [(1,1), (1,2), (2,1), (2,2)]
used = [(2,2), (1,1)]
mat = [[1,2,3], [4,5,6], [7,8,9]]

main :: IO ()
main = do
        putStrLn $ show mat
        putStrLn $ show $ getNeighbour (1,1)
        --putStrLn $ show $ deleteUsedPositions pos used



-- ----------   COMPAS FUNCTIONS    ----------

-- return the positions of the neighbours of the actual position
-- TODO: if neighbour positions already used ?
getNeighbour :: Position -> [Position]
getNeighbour pos@(1, 1) = [(1, 2), (2, 1)]
getNeighbour pos@(1, y) = [(2, y), (1, y+1), (1,y-1)]
getNeighbour pos@(x, 1) = [(x+1, 1), (x, 2), (x-1, 1)]
getNeighbour pos@(x, y) = [(x+1, y), (x, y+1), (x-1, y), (x,y-1)]

-- Get the list of neighbours and the list of already checked positions
-- return list of positions of neighbours without the already used
deleteUsedPositions :: [Position] -> [Position] -> [Position]
deleteUsedPositions neighboursPos usedPos = checkUsed neighboursPos []
    where checkUsed [] newList = newList
          checkUsed (x:xs) newList
            | x `elem` usedPos = checkUsed xs newList
            | otherwise = checkUsed xs (newList ++ [x])

-- ----------                       ----------

-- TODO: last line need one tile more
extendField :: Matrix Weapon -> Prob -> StdGen -> [[Weapon]]
extendField field normalProbList seed =  newMatrix
    where (fieldLists, preSeed) = extendFieldBis field normalProbList seed
          (newLine,newSeed) = generate (length fieldLists) ([], preSeed)
          newMatrix = fieldLists ++ [newLine]
          generate (-1) (list, seed) = (list, seed)
          generate counter (list, seed)
            | counter /= (length fieldLists) && (list!!(length fieldLists - (counter + 1))) == Rock = generate (counter-1) (list ++ [ Rock ], (seed))
            | (fieldLists!!(length fieldLists-1)!!(length fieldLists - counter)) == Rock = generate (counter-1) (list ++ [ Rock ], (seed))
            | otherwise = generate (counter-1) (list ++ [ fst $ randomChooseTile normalProbList seed ], (snd $ randomChooseTile normalProbList seed))

-- TODO: upper check
extendFieldBis :: Matrix Weapon -> Prob -> StdGen -> ([[Weapon]], StdGen)
extendFieldBis field normalProbList seed = (newMatrix, newSeed)
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
