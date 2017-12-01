module Main where

import Player
import Fields

import System.Random
import Control.Monad
import Data.Matrix

-- ---------- EXECUTION TEST PART ----------
matrix = [  [Water, Water, Water],
            [Lava, Water, Lava],
            [Water, Water, Water]]
matrixB = [  [Tile True Water True, Tile False Water False, Tile False Water False],
            [Tile False Lava False, Tile False Water False, Tile False Lava False],
            [Tile False Water False, Tile False Water False, Tile False Portal False]]
tileTest = Tile True (Desert False) False

mat = fromLists matrixB
lineofsigth = 1
probList = [(10, Desert True), (25, Water), (25, Lava), (5, Portal), (20, Desert False)]
-- ----------------------------------------

-- USER PARAMETERS
s = 1 -- Line of sight
m = 15 -- Maximum of water
g = 999 -- initial Seed
t = fromIntegral 10 -- % desert contains a treasure
w = fromIntegral 25 -- % Water tile generation
p = fromIntegral 5 -- % Portal tile generation
l = fromIntegral 10 -- % Lava tile generation without lava adjacent
ll = fromIntegral 70 -- % Lava tile generation with lava adjacent

pl = Player m m 0 1 1 -- Initial Player

probListStandard = [(w, Water), (p, Portal), (l, Lava), ((100 - w - p - l) * (1 - (t/100)), Desert False), ((100 - w - p - l) * (t/100), Desert True)]
probListLavaLac = [(w, Water), (p, Portal), (ll, Lava), ((100 - w - p - l) * (1 - (t/100)), Desert False), ((100 - w - p - l) * (t/100), Desert True)]

main :: IO()
main = do
    gen <- getStdGen -- TODO: mkStdGen

    let (listField, newGen) = initFieldInList probListStandard gen
    let gameField = fromList 5 5 listField

    -- TEST EXTEND FUNCTION --
    let tEST_FIELD = fromLists $ fst $ extendField gameField probListStandard probListLavaLac gen
    --                      --

    -- Launch game
    game pl (discoverTiles tEST_FIELD (xCoord pl, yCoord pl) lineofsigth)
    --TODO: Send new gen

verificationState :: Player -> Matrix Tile -> IO()
verificationState player field = do
    putStrLn $ " "
    if (water player == 0)
        then do putStrLn $ "You died of thirst !"
                putStrLn $ "End of the Game !"
    else if (typeTile (getElem (xCoord player) (yCoord player) field) == Lava)
        then do putStrLn $ "You died in LAVA !"
                putStrLn $ "End of the Game !"
    else if (typeTile (getElem (xCoord player) (yCoord player) field) == Portal)
        then putStrLn $ "Congratz, you find a Portal !"
    else if (typeTile (getElem (xCoord player) (yCoord player) field) == Water)
        then do putStrLn $ "You find a stock of water"
                game (findWater player) field
    else if (typeTile (getElem (xCoord player) (yCoord player) field) == Desert True)
        then do putStrLn $ "You find a treasure"
                game (findTreasure player) field
    else do game player field

game :: Player -> Matrix Tile -> IO()
game player field = do
    -- Ask for the several params
    -- TODO: use argv

    -- TODO: player case verification : WATER - LAVA - TREASURE
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
        "z" -> verificationState up (discoveredField (updatedField up) (xCoord up, yCoord up))
        "s" -> verificationState down (discoveredField (updatedField down) (xCoord down, yCoord down))
        "q" -> verificationState left (discoveredField (updatedField left) (xCoord left, yCoord left))
        "d" -> verificationState right (discoveredField (updatedField right) (xCoord right, yCoord right))
        otherwise -> do putStrLn "----- Error : Please enter movement in zqsd -----"
                        game player field


    return ()
