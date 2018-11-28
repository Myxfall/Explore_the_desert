-- Maximilien Romain
-- 0543411

-- The project runs with argv
-- example of execution :
-- ./main 2 15 900 10 25 5 10 70

module Main where

import Player
import Fields

import System.Environment
import System.Random
import Control.Monad
import Data.Matrix

main :: IO()
main = do

    argv <- getArgs
    -- fromIntegral : We need Double for the calcul of the probabilities
    let s = fromIntegral $ read $ argv!!0  -- Line of sight
    let m = fromIntegral $ read $ argv!!1 -- Maximum of water
    let g = fromIntegral $ read $ argv!!2 -- initial Seed
    let t = fromIntegral $ read $ argv!!3 -- % desert contains a treasure
    let w = fromIntegral $ read $ argv!!5 -- % Water tile generation
    let p = fromIntegral $ read $ argv!!5-- % Portal tile generation
    let l = fromIntegral $ read $ argv!!6 -- % Lava tile generation without lava adjacent
    let ll = fromIntegral $ read $ argv!!7 -- % Lava tile generation with lava adjacent

    let pl = Player m m 0 1 1 -- Initial Player
    let a = checkArgv argv
    putStrLn $ show a

    if not $ checkArgv argv
        then putStrLn $ "Error in the arguments"
    else do let probListStandard = [(w, Water), (p, Portal), (l, Lava), ((100 - w - p - l) * (1 - (t/100)), Desert False), ((100 - w - p - l) * (t/100), Desert True)]
            let probListLavaLac = [(w, Water), (p, Portal), (ll, Lava), ((100 - w - p - l) * (1 - (t/100)), Desert False), ((100 - w - p - l) * (t/100), Desert True)]

            let (listField, newGen) = initFieldInList probListStandard (mkStdGen g)
            let gameField = fromList 5 5 listField

            -- Launch game
            game pl (discoverTiles gameField (xCoord pl, yCoord pl) s) probListStandard probListLavaLac s (mkStdGen g)

-- Check all the parameters
checkArgv :: [String] -> Bool
checkArgv list
    | length list /= 8 = False
    | otherwise = checkArgv' $ map (fromIntegral . read) list
checkArgv' args@[s,m,g,t,w,p,l,ll]
    | w + p + l > 100 = False
    | w + p + ll > 100 = False
    | t > 100 = False
    | w < 0 || p < 0 || l < 0 || l < 0 || s < 0 || m < 0 || g < 0 || t < 0 = False
    | otherwise = True

verificationState :: Player -> Matrix Tile -> Prob -> Prob -> LineOfSight -> StdGen -> IO()
verificationState player field probListStandard probListLavaLac lineofsigth seed = do

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
                game (findWater player) field probListStandard probListLavaLac lineofsigth seed
    else if (typeTile (getElem (xCoord player) (yCoord player) field) == Desert True)
        then do putStrLn $ "You find a treasure"
                game (findTreasure player) (treasureFound field (xCoord player, yCoord player)) probListStandard probListLavaLac lineofsigth seed
    else if (xCoord player + lineofsigth >= nrows field || yCoord player + lineofsigth >= ncols field)
        then game player (fromLists $ fst $ extendField field probListStandard probListLavaLac seed) probListStandard probListLavaLac lineofsigth (snd $ extendField field probListStandard probListLavaLac seed)
    else game player field probListStandard probListLavaLac lineofsigth seed

game :: Player -> Matrix Tile -> Prob -> Prob -> LineOfSight -> StdGen -> IO()
game player field probListStandard probListLavaLac lineofsigth seed = do

    putStrLn $ show field
    putStrLn $ show player

    let inf@(waterSteps, treasureSteps, portalSteps) = compass field (xCoord player, yCoord player)
    if (waterSteps == 100 || treasureSteps == 100 || portalSteps == 100) -- if didnt find the element
        then game player (fromLists $ fst $ extendField field probListStandard probListLavaLac seed) probListStandard probListLavaLac lineofsigth (snd $ extendField field probListStandard probListLavaLac seed)
    else do putStrLn $ "There is water in " ++ show waterSteps ++ ", treasure in " ++ show treasureSteps ++ " and a portal in " ++ show portalSteps ++ " steps"
            putStrLn $ "Player's move : "
            moveDirection <- getLine

            -- Azerty movements : zqsd
            let up = moveUp player
                down = moveDown player
                right = moverRight player
                left = moveLeft player
                updatedField newPlayer = updateField field player newPlayer
                discoveredField newField pos = discoverTiles newField pos lineofsigth
            case moveDirection of
                "w" -> verificationState up (discoveredField (updatedField up) (xCoord up, yCoord up)) probListStandard probListLavaLac lineofsigth seed
                "s" -> verificationState down (discoveredField (updatedField down) (xCoord down, yCoord down)) probListStandard probListLavaLac lineofsigth seed
                "a" -> verificationState left (discoveredField (updatedField left) (xCoord left, yCoord left)) probListStandard probListLavaLac lineofsigth seed
                "d" -> verificationState right (discoveredField (updatedField right) (xCoord right, yCoord right)) probListStandard probListLavaLac lineofsigth seed
                otherwise -> do putStrLn "----- Error : Please enter movement in wasd -----"
                                game player field probListStandard probListLavaLac lineofsigth seed


    return ()
