module Fields where

import Player

import Data.Matrix
import System.Random

nO_COMPASS = -99

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
-- Simple algorithm that matches one percentage of occurrence of tileType and one random number between 0 and 100
-- It works with steps, the smallest numbers are for the first percentage of occurrence, the next for the others
randomChooseTile :: Prob -> StdGen -> (TileType, StdGen)
randomChooseTile probList seed = (generateTile probList rand, newSeed)
    where generateTile ((probTile, tile):xs) rand
            | rand <= probTile = tile
            | otherwise = generateTile xs (rand - probTile)
          (rand, newSeed) = randomR (0,100) seed :: (Double, StdGen)

-- TODO: change. The first two are same always (two same seed for two calls random first)
initFieldInList :: Prob -> StdGen -> ([Tile], StdGen)
initFieldInList probList seed = generate 25 ([ Tile True (fst $ randomChooseTile probList seed) True ], seed)
    where generate 1 (list, seed) = (list, seed)
          generate counter (list, seed) = generate (counter-1) (list ++ [ Tile False (fst $ randomChooseTile probList seed) False ], (snd $ randomChooseTile probList seed))

-- Function that takes the field and the player updated and returns the updated
-- field
updateField :: Matrix Tile -> Player -> Player -> Matrix Tile
updateField field player newPlayer = setElem (Tile True (typeTile (getElem (xCoord newPlayer) (yCoord newPlayer) field)) True) ((xCoord newPlayer),(yCoord newPlayer)) (updatedTest field (xCoord player) (yCoord player))

updatedTest :: Matrix Tile -> Int -> Int -> Matrix Tile
updatedTest field x y = setElem (Tile True (typeTile $ getElem x y field) False)  (x,y) field

-- TODO: discover around the player
--      fun :: Player -> Matrix Tile -> Int (line of sigth) -> Matrix Tile
--      First try, cross around player (x, y+1) (x+1, y) (x, y-1) (x-1, y)
--      Try with "where" and match direction
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
                                      else rightDiscover (setElem (Tile True (typeTile $ getElem x (y+1) field) False) (x,y+counter) field) (x,y) n (counter-1)

downDiscover :: Matrix Tile -> Position -> LineOfSight -> Int -> Matrix Tile
downDiscover field (x,y) n 0 = field
downDiscover field (x,y) n counter = if x == nrows field
                                     then field
                                     else downDiscover (setElem (Tile True (typeTile $ getElem (x+1) y field) False) (x+counter,y) field) (x,y) n (counter-1)

leftDiscover :: Matrix Tile -> Position -> LineOfSight -> Int -> Matrix Tile
leftDiscover field (x,y) n 0 = field
leftDiscover field (x,y) n counter = if y == 1
                                     then field
                                     else leftDiscover (setElem (Tile True (typeTile $ getElem x (y-1) field) False) (x,y-counter) field) (x,y) n (counter-1)

upDiscover :: Matrix Tile -> Position -> LineOfSight -> Int -> Matrix Tile
upDiscover field (x,y) n 0 = field
upDiscover field (x,y) n counter = if x == 1
                                   then field
                                   else upDiscover (setElem (Tile True (typeTile $ getElem (x-1) y field) False) (x-counter,y) field) (x,y) n (counter-1)

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

-- TODO: upper check
extendFieldBis :: Matrix Tile -> Prob -> Prob -> StdGen -> ([[Tile]], StdGen)
extendFieldBis field normalProbList lavaProbList seed = (newMatrix, newSeed)
   where fieldLists = toLists field
         (newMatrix, newSeed) = extendLoop (length fieldLists) ([], seed)
         extendLoop 0 (list, seed) = (list, seed)
         extendLoop counter (list, seed)
           | typeTile (fieldLists!!(length fieldLists - counter)!!(length fieldLists - 1)) == Lava = extendLoop (counter-1) (list ++ [(fieldLists!!(length fieldLists - counter)) ++ [ Tile False (fst $ randomChooseTile lavaProbList seed) False ]], snd $ randomChooseTile lavaProbList seed)
           | otherwise = extendLoop (counter-1) (list ++ [(fieldLists!!(length fieldLists - counter)) ++ [ Tile False (fst $ randomChooseTile normalProbList seed) False ]], snd $ randomChooseTile normalProbList seed)

-- TODO: compass - 2 versions voisin l
-- TODO: play with position and not with actual type Tile
-- TODO: If position used, append in list of used positions
-- TODO: Une liste pour tous les voisins
-- concat $ map getNeighbour [(1,1), (2,2)] -> [(2,1),(1,2),(0,1),(1,0),(3,2),(2,3),(1,2),(2,1)]
-- TODO: need to avoid lava and find water (delete lavaTile in neighbours list)
-- TODO: return 3 paramether (try to remember wich one is found with typle of BOOL, pattern match when all are TRUE)
-- ----------   COMPAS FUNCTIONS    ----------
-- compass :: Matrix Tile -> Position -> Int
-- compass field pos = launch
--     where launch = check (getNeighbour field pos) [pos] [] 1
--           check _ _ _ 50 = nO_COMPASS
--           check [] checkedList nextNeighbour res = check (deleteUsedPositions (concat $ map (getNeighbour field) nextNeighbour) checkedList) checkedList [] (res+1)
--           check ((x, y):xs) checkedList nextNeighbour res
--             | typeTile (getElem x y field) == Water = res
--             | otherwise = check xs (checkedList ++ [(x, y)]) (nextNeighbour ++ [(x, y)]) res


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
-- ----------                       ----------


-- TODO: Extend Matrix, new discover tiles
-- extendField :: Matrix Tile -> Prob -> Prob -> [Tile]
-- extendField field normalProbList lavaProbList = genOne $ toLists field
--     where genOne fieldLists = fieldLists!!

-- lineField :: [Tile] -> Player -> Int -> Int -> [Tile]
-- lineField (c:[]) player x y = if xCoord player == x && yCoord player y
--                                   then unitField c True
--                                   else unitField c False
-- lineField (c:cs) player x y
--
-- unitField :: Tile -> Bool -> Tile
-- unitField tile player = if isRevealed tile
--                             then if player
--                                 then Tile True (typeTile tile) True
--                                 else Tile True (typeTile tile) False
--                             else Tile False (typeTile tile) False
--

-- Exemple records syntax
test :: Tile -> String
test l = if isRevealed l
    then "IsRevealed True"
    else "not"
