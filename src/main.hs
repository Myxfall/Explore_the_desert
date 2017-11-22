module Main where

import Player
import Fields

import System.Random
import Control.Monad
import Data.Matrix

-- ---------- EXECUTION TEST PART ----------
pl = Player 10 15 0 1 1
matrix = [  [Water, Water, Water],
            [Lava, Water, Lava],
            [Water, Water, Water]]
matrixB = [  [Tile True Water True, Tile False Water False, Tile False Water False],
            [Tile False Lava False, Tile False Water False, Tile False Lava False],
            [Tile False Water False, Tile False Water False, Tile False Water False]]
tileTest = Tile True (Desert False) False

mat = fromLists matrixB
lineofsigth = 1
-- ----------------------------------------
main :: IO()
main = game pl mat


game :: Player -> Matrix Tile -> IO()
game player field = do
    -- Ask for the several params
    -- TODO: use argv

    --putStrLn $ "Game starts !"
    putStrLn $ show field
    putStrLn $ show player

    putStrLn $ "Player's move : "
    moveDirection <- getLine

    -- Azert movements : zqsd
    -- TODO: try to return a function
    -- TODO: try with "let" compute once, call twice
    -- TODO: where with pattern matching for the selection function
    let up = moveUp player
        down = moveDown player
        right = moverRight player
        left = moveLeft player
        updatedField newPlayer = updateField field player newPlayer
        discoveredField newField pos = discoverTiles newField pos lineofsigth
    case moveDirection of
        -- "z" -> do let testUp = moveUp player
        --          game (testUp) (updateField field player (testUp))
        "z" -> game up (discoveredField (updatedField up) (xCoord up, yCoord up))
        "s" -> game down (discoveredField (updatedField down) (xCoord down, yCoord down))
        "q" -> game left (discoveredField (updatedField left) (xCoord left, yCoord left))
        "d" -> game right (discoveredField (updatedField right) (xCoord right, yCoord right))
        otherwise -> do putStrLn "----- Error : Please enter movement in zqsd -----"
                        game player field


    return ()

-- Test function
test :: String -> IO()
test msg = do
    putStrLn $ "Votre message est " ++  msg

printBoard :: [[Integer]] -> String
printBoard a = show (a!!0!!0) ++ show (a!!0!!1) ++ show (a!!0!!2)

printBoardF :: [[Integer]] -> String
printBoardF [] = ""
printBoardF (x:xs) = show x ++ "\n" ++ (printBoardF xs)
