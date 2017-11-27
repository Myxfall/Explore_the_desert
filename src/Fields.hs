module Fields where

import Player

import Data.Matrix
import System.Random


type Position = (Int,Int)
type LineOfSight = Int
type Prob = [(Double, TileType)]
data TileType = Desert Bool | Water | Lava | Portal deriving (Show)
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

-- -- TODO: change this
-- randomWeapon :: [(Double, TileType)] -> StdGen -> (TileType, StdGen)
-- randomWeapon chanceList seed = (weapChoice, newSeed)
--     where (rand, newSeed) = randomR (0,100) seed :: (Double, StdGen)
--           weapChoice = choisit chanceList rand
--           choisit [(prob, weap)] _ = weap
--           choisit ((prob, weap):xs) rand
--             | rand <= prob = weap
--             | otherwise = choisit xs $ rand - prob

-- WARNING: if total % not 100: problems
randomChooseTile :: Prob -> StdGen -> (TileType, StdGen)
randomChooseTile probList seed = (generateTile probList rand, newSeed)
    where (rand, newSeed) = randomR (0,100) seed :: (Double, StdGen)
          generateTile ((probTile, tile):xs) rand
            | rand <= probTile = tile
            | otherwise = generateTile xs (rand - probTile)

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
