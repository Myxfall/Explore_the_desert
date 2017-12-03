-- Maximilien Romain
-- 0543411

module Fields where

import Player

import Data.Matrix
import System.Random

type Position = (Int,Int)
type LineOfSight = Int
type Prob = [(Double, TileType)]
data TileType = Desert Bool | Water | Lava | Portal deriving (Show, Eq)
data Tile = Tile {  isRevealed :: Bool,
                    typeTile :: TileType,
                    playerOn :: Bool}

--instance field show
instance Show Tile where
    show (Tile False _ _) = "*"
    show (Tile True Water False) = "~" -- ~
    show (Tile True Lava False) = "!"
    show (Tile True (Desert False) False) = "."
    show (Tile True (Desert True) False) = "€"
    show (Tile True Portal False) = "O"
    show (Tile True _ True) = "P"

-- WARNING: if total % not 100: problems
-- Take the list of probabilites and the seed and return one tileType with the new Seed
-- Simple algorithm that matches one percentage of occurrence of tileType with one random number between 0 and 100
-- It works with steps, the smallest numbers are for the first percentage of occurrence, the next for the others
randomChooseTile :: Prob -> StdGen -> (TileType, StdGen)
randomChooseTile probList seed = (generateTile probList randNumber, newSeed)
    where generateTile ((probTile, tile):xs) randNumber
            | randNumber <= probTile = tile -- if randNumber matches the steps, return the tile. if we have a tile generated with 25%, a num between 1 to 25 generates the tile
            | otherwise = generateTile xs (randNumber - probTile) -- otherwise check the next step
          (randNumber, newSeed) = randomR (1,100) seed :: (Double, StdGen) -- 1 to 100 makes 100 possibilities. 0 to 100 makes 101 possibilities

-- Initialise the field for the beginning of the game
initFieldInList :: Prob -> StdGen -> ([Tile], StdGen)
initFieldInList probList seed = generate 25 ([ Tile True (fst $ randomChooseTile probList seed) True ], snd $ randomChooseTile probList seed)
    where generate 1 (list, seed) = (list, seed)
          generate counter (list, seed) = generate (counter-1) (list ++ [ Tile False (fst $ randomChooseTile probList seed) False ], (snd $ randomChooseTile probList seed))

-- Function that takes the field and the player updated and returns the updated
-- field
updateField :: Matrix Tile -> Player -> Player -> Matrix Tile
updateField field player newPlayer = setElem (Tile True (typeTile (getElem (xCoord newPlayer) (yCoord newPlayer) field)) True) ((xCoord newPlayer),(yCoord newPlayer)) (updatedTest field (xCoord player) (yCoord player))

updatedTest :: Matrix Tile -> Int -> Int -> Matrix Tile
updatedTest field x y = setElem (Tile True (typeTile $ getElem x y field) False)  (x,y) field

-- Discover with the line of sight the tiles around the player
discoverTiles :: Matrix Tile -> Position -> LineOfSight -> Matrix Tile
discoverTiles field (x,y) n = right
    where right = rightDiscover down (x,y) n n
          down  = downDiscover left (x,y) n n
          left = leftDiscover up (x,y) n n
          up = upDiscover field (x,y) n n

rightDiscover :: Matrix Tile -> Position -> LineOfSight -> Int -> Matrix Tile
rightDiscover field (x,y) n 0 = field
rightDiscover field (x,y) n counter = if y == ncols field
                                        then field
                                      else if y+counter > (ncols field)
                                          then rightDiscover field (x,y) n ((ncols field) - y)
                                      else rightDiscover (setElem (Tile True (typeTile $ getElem x (y+counter) field) False) (x,y+counter) field) (x,y) n (counter-1)

downDiscover :: Matrix Tile -> Position -> LineOfSight -> Int -> Matrix Tile
downDiscover field (x,y) n 0 = field
downDiscover field (x,y) n counter = if x == nrows field
                                        then field
                                     else if x+counter > (nrows field)
                                         then downDiscover field (x,y) n ((nrows field) - x)
                                     else downDiscover (setElem (Tile True (typeTile $ getElem (x+counter) y field) False) (x+counter,y) field) (x,y) n (counter-1)

leftDiscover :: Matrix Tile -> Position -> LineOfSight -> Int -> Matrix Tile
leftDiscover field (x,y) n 0 = field
leftDiscover field (x,y) n counter = if y == 1
                                        then field
                                    else if (y - counter) <= 0
                                        then field
                                    else if y+counter > (ncols field)
                                        then rightDiscover field (x,y) n ((ncols field) - y)
                                     else leftDiscover (setElem (Tile True (typeTile $ getElem x (y-counter) field) False) (x,y-counter) field) (x,y) n (counter-1)

upDiscover :: Matrix Tile -> Position -> LineOfSight -> Int -> Matrix Tile
upDiscover field (x,y) n 0 = field
upDiscover field (x,y) n counter = if x == 1
                                        then field
                                    else if (x - counter) <= 0
                                        then field
                                   else if x+counter > (nrows field)
                                       then downDiscover field (x,y) n ((nrows field) - x)
                                   else upDiscover (setElem (Tile True (typeTile $ getElem (x-counter) y field) False) (x-counter,y) field) (x,y) n (counter-1)

-- IMPORTANT NOTE: I could have used extendTo or setSize from Data.Matrix (I really could ...), but I discovered it too late and I didn't have time to change it.
-- Two functions that takes the field and the list of probabilities and return the extended field
extendField :: Matrix Tile -> Prob -> Prob -> StdGen -> ([[Tile]], StdGen)
extendField field normalProbList lavaProbList seed =  (newMatrix, newSeed)
   where (fieldLists, preSeed) = extendFieldBis field normalProbList lavaProbList seed
         (newLine,newSeed) = generate (length fieldLists) ([], preSeed)
         newMatrix = fieldLists ++ [newLine]
         generate (-1) (list, seed) = (list, seed)
         generate counter (list, seed)
           | counter /= (length fieldLists) && typeTile (list!!(length fieldLists - (counter + 1))) == Lava = generate (counter-1) (list ++ [ Tile False (fst $ randomChooseTile lavaProbList seed) False ], snd $ randomChooseTile lavaProbList seed)
           | typeTile (fieldLists!!(length fieldLists-1)!!(length fieldLists - counter)) == Lava = generate (counter-1) (list ++ [ Tile False (fst $ randomChooseTile lavaProbList seed) False ], snd $ randomChooseTile lavaProbList seed)
           | otherwise = generate (counter-1) (list ++ [ Tile False (fst $ randomChooseTile normalProbList seed) False ], snd $ randomChooseTile normalProbList seed)

extendFieldBis :: Matrix Tile -> Prob -> Prob -> StdGen -> ([[Tile]], StdGen)
extendFieldBis field normalProbList lavaProbList seed = (newMatrix, newSeed)
   where fieldLists = toLists field
         (newMatrix, newSeed) = extendLoop (length fieldLists) ([], seed)
         extendLoop 0 (list, seed) = (list, seed)
         extendLoop counter (list, seed)
           | typeTile (fieldLists!!(length fieldLists - counter)!!(length fieldLists - 1)) == Lava = extendLoop (counter-1) (list ++ [(fieldLists!!(length fieldLists - counter)) ++ [ Tile False (fst $ randomChooseTile lavaProbList seed) False ]], snd $ randomChooseTile lavaProbList seed)
           | otherwise = extendLoop (counter-1) (list ++ [(fieldLists!!(length fieldLists - counter)) ++ [ Tile False (fst $ randomChooseTile normalProbList seed) False ]], snd $ randomChooseTile normalProbList seed)

-- Takes the field and the player's position and return 3 integers : the number of steps to access the nearest water / treasure / portal
compass :: Matrix Tile -> Position -> (Int, Int, Int)
compass field pos = launch
    where launch = check (getNeighbour field pos) [pos] [] (1,1,1) (False, False, False)
          check _ _ _ res@(100, _, _) _ = res
          check _ _ _ res@(_, 100, _) _ = res
          check _ _ _ res@(_, _, 100) _ = res
          check _ _ _ stepF (True, True, True) = stepF
          check [] checkedList nextNeighbour (sW, sT, sP) ifFound@(ifW, ifT, ifP) = check (deleteUsedPositions (concat $ map (getNeighbour field) nextNeighbour) checkedList) checkedList [] (if ifW then sW else sW+1, if ifT then sT else sT+1, if ifP then sP else sP+1) ifFound
          check ((x, y):xs) checkedList nextNeighbour res ifFound@(ifW, ifT, ifP)
            | typeTile (getElem x y field) == Water = check xs (checkedList ++ [(x, y)]) (nextNeighbour ++ [(x, y)]) res (True, ifT, ifP)
            | typeTile (getElem x y field) == Desert True = check xs (checkedList ++ [(x, y)]) (nextNeighbour ++ [(x, y)]) res (ifW, True, ifP)
            | typeTile (getElem x y field) == Portal = check xs (checkedList ++ [(x, y)]) (nextNeighbour ++ [(x, y)]) res (ifW, ifT, True)
            | otherwise = check xs (checkedList ++ [(x, y)]) (nextNeighbour ++ [(x, y)]) res ifFound


-- Takes the list of neighbours and removes neighbours who are lava
deleteLavaNeighbour :: [Position] -> Matrix Tile -> [Position]
deleteLavaNeighbour neighboursPos field = loop neighboursPos field []
    where loop [] field newList = newList
          loop ((x, y):xs) field newList
            | typeTile (getElem x y field) == Lava = loop xs field newList
            | otherwise = loop xs field (newList ++ [(x, y)])

-- return the positions of the neighbours of the actual position
getNeighbour :: Matrix Tile -> Position -> [Position]
getNeighbour field pos = deleteLavaNeighbour (getList pos) field
    where getList (1, 1) = [(1, 2), (2, 1)]
          getList (1, y)
            | y == ncols field = [(2, y), (1, y-1)]
            | otherwise = [(2, y), (1, y-1), (1, y+1)]
          getList (x, 1)
            | x == ncols field = [(x, 2), (x-1, 1)]
            | otherwise = [(x, 2), (x-1, 1), (x+1, 1)]
          getList (x, y)
            | x == nrows field && y == ncols field = [(x-1, y), (x,y-1)]
            | x == nrows field = [(x, y+1), (x-1, y), (x,y-1)]
            | y == ncols field = [(x+1, y), (x-1, y), (x,y-1)]
            | otherwise = [(x+1, y), (x, y+1), (x-1, y), (x,y-1)]

-- Get the list of neighbours and the list of already checked positions
-- return list of positions of neighbours without the already used
deleteUsedPositions :: [Position] -> [Position] -> [Position]
deleteUsedPositions neighboursPos usedPos = checkUsed neighboursPos []
    where checkUsed [] newList = newList
          checkUsed (x:xs) newList
            | x `elem` usedPos = checkUsed xs newList
            | otherwise = checkUsed xs (newList ++ [x])

-- When you find a treasure, we need to replace the tile by an empty Desert
treasureFound :: Matrix Tile -> Position -> Matrix Tile
treasureFound field pos = setElem (Tile True (Desert False) True) pos field
