
module Player where

data Player = Player { water :: Int,
            maxWater :: Int,
            treasure :: Int,
            xCoord :: Int,
            yCoord :: Int}

instance Show Player where
    --show (Player 0 _ _ _ _) = "Player's dead"
    show (Player instantWater maxWater score x y) = "Player in (" ++ show x ++ "," ++ show y ++  ") water: " ++ show instantWater ++ "/" ++ show maxWater ++ " treasure : " ++ show score

type Water = Int

-- Player control movement

-- TODO: loseWater with movements
-- TODO: Gain something depend on the tile (import Field)

moveUp :: Player -> Player
moveUp p@(Player _ _ _ 1 _) = p
moveUp p@(Player a b c x y) = Player (a-1) b c (x-1) y

moveDown :: Player -> Player
moveDown p@(Player a b c x y) = Player (a-1) b c (x+1) y

moveLeft :: Player -> Player
moveLeft p@(Player _ _ _ _ 1) = p
moveLeft p@(Player a b c x y) = Player (a-1) b c x (y-1)

moverRight :: Player -> Player
moverRight p@(Player a b c x y) = Player (a-1) b c x (y+1)

-- moveUp :: Player -> TileType -> Maybe (Player, TileType)
-- moveUp p@(Player _ _ _ _ 0) f = Just (p, f)
-- moveUp p@(Player a b c x y) f = Just (Player a b c x (y-1), f)
--
-- moveDown :: Player -> TileType -> Maybe (Player, TileType)
-- moveDown p@(Player a b c x y) f = Just (Player a b c x (y+1), f)
--
-- moverRight :: Player -> TileType -> Maybe (Player, TileType)
-- moverRight p@(Player a b c x y) f = Just (Player a b c (x+1) y, f)
--
-- moveLeft :: Player -> TileType -> Maybe (Player, TileType)
-- moveLeft p@(Player _ _ _ 0 _) f = Just (p, f)
-- moveLeft p@(Player a b c x y) f = Just (Player a b c (x-1) y, f)

findTreasure :: Player -> Player
findTreasure (Player water maxWater treasure x y) = Player water maxWater (treasure+1) x y

findWater :: Player  -> Player
findWater (Player water maxWater treasure x y) = Player maxWater maxWater treasure x y

loseWater :: Player -> Water -> Player
loseWater (Player water maxWater treasure x y) waterLose = Player (water - waterLose) maxWater treasure x y
