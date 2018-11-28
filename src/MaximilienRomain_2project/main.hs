-- Maximilien Romain
-- 0543411

-- The project runs with argv
-- example of execution :
-- ./main 2 15 900 10 25 5 10 70
-- ./main 2 10 99 20 50 10 10 10

module Main where

import Player
import Fields
import Save
import ParserFile
import Worm
import Gui

import System.Environment
import System.Random
import System.Exit
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Matrix

import qualified Graphics.Gloss.Interface.IO.Game as Gloss

askNewGameOrLoad :: IO String
askNewGameOrLoad = do putStrLn "Do you want go start a new Game (0) or load a game (1) ?"
                      res <- getLine
                      return res

main :: IO()
main = do

    res <- askNewGameOrLoad

    if res == "0"
        then do putStrLn "Creating New Game"
                launchNewGame
        else do putStrLn "You are going to load a new game"
                loadGame

loadGame :: IO()
loadGame = do putStrLn "Filename of saved game : "
              filename <- getLine
              loadedFile <- readFile filename

              let parsedFile = parse loadedFile
              putStrLn $ show parsedFile
              let loadedParamsGame@(water, s, m, g, t, w, p, l, ll, x, y) =  extractOptions parsedFile
              let loadedPositionsGame = extractPosition parsedFile
              let loadedWorms@(emerging, disappearing) = extractWorms parsedFile

              let emergingWorms = createWormsFromPositions emerging True x
              let disappearingWorms = createWormsFromPositions disappearing False x

              let worms =  createWormsFromPositions emerging True x ++ createWormsFromPositions disappearing False x
              putStrLn $ show  worms

              let playerPosition = loadedPositionsGame !! 0 !! 0
              let revealedPositions = loadedPositionsGame !! 1
              let collectedPositions = loadedPositionsGame !! 2

              let newt = fromIntegral t
              let neww = fromIntegral w
              let newp = fromIntegral p
              let newl = fromIntegral l
              let newll = fromIntegral ll
              let newPlayer = Player water m 0 (fst playerPosition) (snd playerPosition)

              let probListStandard = [(neww, Water), (newp, Portal), (newl, Lava), ((100 - neww - newp - newl) * (1 - (newt/100)), Desert False), ((100 - neww - newp - newl) * (newt/100), Desert True)]
              let probListLavaLac = [(neww, Water), (newp, Portal), (newll, Lava), ((100 - neww - newp - newl) * (1 - (newt/100)), Desert False), ((100 - neww - newp - newl) * (newt/100), Desert True)]



              let (listField, newGen) = initFieldInList probListStandard (mkStdGen g)
              let (gameField, newSeed) = generateLoadedField (fromList 5 5 listField) revealedPositions probListStandard probListLavaLac newGen
              let updatedField = foldr removeTreasure (newPlayerPosition newPlayer $ foldr revealTile gameField revealedPositions) collectedPositions

              let updatedPlayer = foldr loadCollectedTreasure newPlayer collectedPositions
              let fieldWithWorms = updateWormsOnField worms updatedField

              -- Launch game
              let options = (s, m, g, newt, neww, newp, newl, newll, x, y)
              gameGUI (updatedPlayer, fieldWithWorms, probListStandard, probListLavaLac, s, (mkStdGen g), options, worms)


launchNewGame :: IO()
launchNewGame = do
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

    let x =  fromIntegral $ read $ argv!!8 -- Worms lenght
    let y =  fromIntegral $ read $ argv!!9 -- % worm generation

    let pl = Player m m 0 1 1 -- Initial Player
    let a = checkArgv argv

    if not $ checkArgv argv
        then putStrLn $ "Error in the arguments"
    else do let probListStandard = [(w, Water), (p, Portal), (l, Lava), ((100 - w - p - l) * (1 - (t/100)), Desert False), ((100 - w - p - l) * (t/100), Desert True)]
            let probListLavaLac = [(w, Water), (p, Portal), (ll, Lava), ((100 - w - p - l) * (1 - (t/100)), Desert False), ((100 - w - p - l) * (t/100), Desert True)]

            let (listField, newGen) = initFieldInList probListStandard (mkStdGen g)
            let gameField = fromList 5 5 listField

            -- Launch game
            let options = (s, m, g, t, w, p, l, ll, x, y)
            gameGUI (pl, (discoverTiles gameField (xCoord pl, yCoord pl) s), probListStandard, probListLavaLac, s, newGen, options, [newWorm (2,2) (1,0) 3])


-- Function that manage the spawning rate of worms
wormSpawning :: [Worm] -> Int -> Int -> Matrix Tile -> StdGen -> ([Worm], StdGen, Bool)
wormSpawning worms wormsRate wormsLength field gen =
    let (ifSpawn, seed) = randomWormSpawning wormsRate gen  -- if a new worm will spawn
        (pos, newSeed) = randomPositionOnField field seed   -- if new worms, return the new position
        (dir, nnewSeed) = randomDirection newSeed           -- direction of the new worm
        cleanedWorms = worms
    in  if ifSpawn
            then if typeTile (field ! pos) == Desert False
                    then (cleanedWorms ++ [newWorm pos (0,-1) wormsLength], nnewSeed, True)
                    else (cleanedWorms, nnewSeed, False)
        else (cleanedWorms, nnewSeed, False)

-- Update the worms in the field
wormsUpdate :: Matrix Tile -> [Worm] -> [Worm]
wormsUpdate field worms = loop worms
    where   loop [] = []
            loop [worm] = [wormUpdate field worm]
            loop (worm:xs) = [wormUpdate field worm] ++ loop xs

wormUpdate :: Matrix Tile -> Worm -> Worm
wormUpdate field worm = updatedWorm x y x1 y1
    where   wormNext@(x, y) = wormNextMove worm
            wormDir@(x1,y1) = wormDirection worm
            updatedWorm a b c d
                | a == 1 && b == 1 = wormMovement worm False
                | a == 1 && c == -1 = wormMovement worm False
                | b == 1 && d == -1 = wormMovement worm False
                | typeTile (getElem a b field) == Desert False =  wormMovement worm True
                | typeTile (getElem a b field) /= Desert False =  wormMovement worm False

-- Check all the parameters
checkArgv :: [String] -> Bool
checkArgv list
    | length list /= 10 = False
    | otherwise = checkArgv' $ map (fromIntegral . read) list
checkArgv' args@[s,m,g,t,w,p,l,ll,x,y]
    | w + p + l > 100 = False
    | w + p + ll > 100 = False
    | t > 100 = False
    | w < 0 || p < 0 || l < 0 || l < 0 || s < 0 || m < 0 || g < 0 || t < 0 = False
    | otherwise = True

verificationState :: Player -> Matrix Tile -> Prob -> Prob -> LineOfSight -> StdGen -> Options -> [Worm] -> IO()
verificationState player field probListStandard probListLavaLac lineofsigth seed options worms = do

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
                game (findWater player) field probListStandard probListLavaLac lineofsigth seed options worms
    else if (typeTile (getElem (xCoord player) (yCoord player) field) == Desert True)
        then do putStrLn $ "You find a treasure"
                game (findTreasure player) (treasureFound field (xCoord player, yCoord player)) probListStandard probListLavaLac lineofsigth seed options worms
    else if (xCoord player + lineofsigth >= nrows field || yCoord player + lineofsigth >= ncols field)
        then game player (fromLists $ fst $ extendField field probListStandard probListLavaLac seed) probListStandard probListLavaLac lineofsigth (snd $ extendField field probListStandard probListLavaLac seed) options worms
    else game player field probListStandard probListLavaLac lineofsigth seed options worms

-- Console Version of the game
game :: Player -> Matrix Tile -> Prob -> Prob -> LineOfSight -> StdGen -> Options -> [Worm] -> IO()
game player field probListStandard probListLavaLac lineofsigth seed options@(s, m, g, t, w, p, l, ll, x, y) worms = do

    putStrLn $ show field
    putStrLn $ show player

    let inf@(waterSteps, treasureSteps, portalSteps) = compass field (xCoord player, yCoord player)
    if (waterSteps == 100 || treasureSteps == 100 || portalSteps == 100) -- if didnt find the element
        then game player (fromLists $ fst $ extendField field probListStandard probListLavaLac seed) probListStandard probListLavaLac lineofsigth (snd $ extendField field probListStandard probListLavaLac seed) options worms
    else do putStrLn $ "There is water in " ++ show waterSteps ++ ", treasure in " ++ show treasureSteps ++ " and a portal in " ++ show portalSteps ++ " steps"
            putStrLn $ "Player's move : "
            moveDirection <- getLine

            -- Azerty movements : zqsd
            let up = moveUp player
                down = moveDown player
                right = moverRight player
                left = moveLeft player

                --wormsMov = cleanWorms [wormUpdate field test]
                wormsMov =  wormsUpdate field ( cleanWorms worms)
                (newWorms, newSeed, _) = wormSpawning wormsMov y x field seed
                fieldWithWorms = updateWormsOnField newWorms (cleanWormsOnField field)

                updatedField newPlayer = updateField fieldWithWorms player newPlayer
                discoveredField newField pos = discoverTiles newField pos lineofsigth

            case moveDirection of
                "w" -> verificationState up (discoveredField (updatedField up) (xCoord up, yCoord up)) probListStandard probListLavaLac lineofsigth seed options wormsMov
                "s" -> verificationState down (discoveredField (updatedField down) (xCoord down, yCoord down)) probListStandard probListLavaLac lineofsigth seed options wormsMov
                "a" -> verificationState left (discoveredField (updatedField left) (xCoord left, yCoord left)) probListStandard probListLavaLac lineofsigth seed options wormsMov
                "d" -> verificationState right (discoveredField (updatedField right) (xCoord right, yCoord right)) probListStandard probListLavaLac lineofsigth seed options wormsMov
                "save" -> saveGame options player field worms
                otherwise -> do putStrLn "----- Error : Please enter movement in wasd -----"
                                game player field probListStandard probListLavaLac lineofsigth seed options wormsMov


    return ()

-- Gui version of the game
gameGUI :: GameState -> IO ()
gameGUI gameState@(player, field, probListStandard, probListLavaLac, lineofsigth, seed, options, worms)
    = do let emptyVar = newEmptyTMVarIO
         a <- emptyVar
         b <- emptyVar
         c <- emptyVar
         let guiUpdate gameState@(_, f, _, _, _, _, _, _) keyMovement = if fst $ verificationStateGUI gameState
                                                                               then gameGuiConcurrentUpdate gameState keyMovement a b c
                                                                               else return gameState

             eventsKey (Gloss.EventKey (Gloss.Char 'w') Gloss.Up _ _) gameState = guiUpdate gameState (0, -1)
             eventsKey (Gloss.EventKey (Gloss.Char 'a') Gloss.Up _ _) gameState = guiUpdate gameState (-1, 0)
             eventsKey (Gloss.EventKey (Gloss.Char 's') Gloss.Up _ _) gameState = guiUpdate gameState (0 , 1)
             eventsKey (Gloss.EventKey (Gloss.Char 'd') Gloss.Up _ _) gameState = guiUpdate gameState (1, 0)
             eventsKey (Gloss.EventKey (Gloss.Char 'p') Gloss.Up _ _) gameState = do saveGame options player field worms
                                                                                     return gameState
             eventsKey (Gloss.EventKey (Gloss.SpecialKey Gloss.KeyEsc) Gloss.Up _ _) _ = System.Exit.exitSuccess
             eventsKey _ gameState = return gameState

         mapM_ (\x -> forkIO (concurrentWorms x a b c)) worms
         Gloss.playIO (Gloss.InWindow "Desert" (round windowsize, round windowsize) (500, 500)) Gloss.white 10 gameState printGame eventsKey (\_ g -> return g)


         return ()

gameGuiUpdate :: GameState -> Position -> IO GameState
gameGuiUpdate gameState@(player, field, probListStandard, probListLavaLac, lineofsigth, seed, options@(s, m, g, t, w, p, l, ll, x, y), worms) dir
    = do let movedPlayer = movePlayer player dir

             (extendedField, newSeed) = extendFieldIfNeeded gameState

             wormsMov =  wormsUpdate extendedField ( cleanWorms worms)
             (newWorms, lastSeed, _) = wormSpawning wormsMov y x extendedField newSeed
             fieldWithWorms = updateWormsOnField newWorms (cleanWormsOnField extendedField)

             updatedField newPlayer = updateField fieldWithWorms player newPlayer
             discoveredField newField pos = discoverTiles newField pos lineofsigth

             newGameState = (movedPlayer, (discoveredField (updatedField movedPlayer) (xCoord movedPlayer, yCoord movedPlayer)), probListStandard, probListLavaLac, lineofsigth, lastSeed, options, wormsMov)
         --putStrLn $ show extendedField
         return newGameState

-- Explaination from StackOverflow's Subject
-- TMVar is a reference to a slot that threads can use to communicate. It can be created holding a value, or empty. You can put a value into it, which if already filled blocks until someone else empties it; or you can take a value from it, which if already empty blocks until someone fills it. It's obviously akin to an MVar, but for many common uses it might be simpler to think of it as a single-element queue used for a communicating producer/consumer pair.
--
-- In short, TVar is general shared state, use it if you want atomic updates to data from arbitrary places. TMVar is a synchronization primitive, use it if you want a thread to wait until something becomes available, while another waits for something to be needed.

-- NOTE: I then used TMvar in order to make my threads waiting for the user inputs. We do not want that worms are being updated while the player did not make any movement
-- Because threads with TMvar will wait if they try to take a VAR inside a TMVAR if it is already full. Same with the PUT, if TMVAR is not empty, the thread is going to wait.

-- Gui & concurrent Version of the GUI. This Version has some crash problems
gameGuiConcurrentUpdate :: GameState -> Position -> TMVar (Int, Matrix Tile) -> TMVar () -> TMVar [Worm] -> IO GameState
gameGuiConcurrentUpdate gameState@(player, field, probListStandard, probListLavaLac, lineofsigth, seed, options@(s, m, g, t, w, p, l, ll, x, y), worms) dir wormsData isFinished wormsThread
    = do putStrLn "Begin of turn"
         atomically (putTMVar wormsThread [])
         putStrLn $ "\nBefore update : " ++ show worms
         when (not (length worms == 0)) $ do atomically (putTMVar wormsData (length worms, field))
                                             putStrLn "Try finished"
                                             atomically (takeTMVar isFinished)
                                             putStrLn "Took Finished"
         wormsMov <- atomically (takeTMVar wormsThread)
         putStrLn $ "After Update " ++ show wormsMov

         let movedPlayer = movePlayer player dir
         let (extendedField, newSeed) = extendFieldIfNeeded gameState
         let wormsMovCleaned = cleanWorms wormsMov
         --let (newWorms, lastSeed, ifSpawn) = wormSpawning wormsMovCleaned y x extendedField newSeed
         let (newWorms, lastSeed, ifSpawn) = (wormsMovCleaned, newSeed, False)
         let fieldWithWorms = updateWormsOnField newWorms (cleanWormsOnField extendedField)

         let updatedField newPlayer = updateField fieldWithWorms player newPlayer
         let discoveredField newField pos = discoverTiles newField pos lineofsigth

         let newGameState = (movedPlayer, (discoveredField (updatedField movedPlayer) (xCoord movedPlayer, yCoord movedPlayer)), probListStandard, probListLavaLac, lineofsigth, newSeed, options, newWorms)
         when ifSpawn (mapM_ (\w -> forkIO (concurrentWorms w wormsData isFinished wormsThread)) [last newWorms])

         putStrLn "End of turn"
         return newGameState


concurrentWorms :: Worm -> TMVar (Int, Matrix Tile) -> TMVar () -> TMVar [Worm] -> IO ()
concurrentWorms worm wormsData isFinished wormsThread = contextThread worm
    where contextThread wormThread = do putStrLn "Worm entry"
                                        (wormsNumber, actualField) <- atomically (takeTMVar wormsData)
                                        putStrLn "Took data"

                                        putStrLn "Before update worm"
                                        putStrLn $ show wormThread
                                        putStrLn $ show actualField
                                        let updatedWorm = wormUpdate actualField wormThread
                                        putStrLn "After update worm"
                                        putStrLn $ show updatedWorm
                                        putStrLn $ show actualField

                                        updatedWorms <- atomically (takeTMVar wormsThread)
                                        atomically (putTMVar wormsThread (updatedWorms ++ [updatedWorm]))
                                        putStrLn $ show wormsNumber
                                        if wormsNumber > 1
                                            then do atomically (putTMVar wormsData (wormsNumber - 1, actualField))
                                                    putStrLn "first"
                                            else do atomically (putTMVar isFinished ())
                                                    putStrLn "Second"
                                        when (isStillAlive wormThread) $ contextThread updatedWorm
