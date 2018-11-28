-- Maximilien Romain
-- 0543411

module Gui where

import Player
import Fields
import Save
import ParserFile
import Worm
import Data.Matrix


import qualified Graphics.Gloss.Data.Bitmap as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

windowsize, mapSize, tileSize, imageSize :: Float
windowsize = mapSize + 100::Float
mapSize = 500
tileSize = 15
imageSize = tileSize / 2 -- Because we use circle radius


printGame :: GameState -> IO Gloss.Picture
printGame gameState@(player, _, _, _, _, _, _, _)
  = return (Gloss.pictures [ Gloss.translate ((tileSize -  mapSize) / 2) (( windowsize - tileSize) / 2) $ Gloss.pictures [printMap gameState, printPlayer player],
                             Gloss.translate 0 (-  mapSize / 2) $ printPlayerInformations gameState])

-- Print the map on the screen
printMap :: GameState -> Gloss.Picture
printMap gameState@(_, field, _, _, _, _, _, _)
    = Gloss.pictures $ map (\pos -> printTileAtPos pos field) (getRevealedTiles field)

-- Print the player on the map
printPlayer :: Player -> Gloss.Picture
printPlayer player@(Player water maxWater treasure x y)
    = Gloss.translate xGloss yGloss $ Gloss.color Gloss.black (Gloss.circleSolid imageSize)
    where (xGloss, yGloss) = positionToGloss (x,y)

-- Print the player informations, like water, treasures and the compass
printPlayerInformations :: GameState -> Gloss.Picture
printPlayerInformations gameState@(player, field, _, _, _, _, _, _)
    = Gloss.pictures [Gloss.translate (- mapSize / 2) 0 $ Gloss.Scale 0.12 0.12 $ Gloss.Text $ compassText,
                      Gloss.translate (- mapSize / 2) (-25) $ Gloss.Scale 0.12 0.12 $ Gloss.Text $ playerInformations]
      where (waterSteps, treasureSteps, portalSteps) = compass field (xCoord player, yCoord player)
            compassText = "There is water in " ++ show waterSteps ++ ", treasure in " ++ show treasureSteps ++ " and a portal in " ++ show portalSteps ++ " steps"
            playerInformations = snd $ verificationStateGUI gameState

printTile :: Tile -> Gloss.Picture
printTile (Tile True Water False)           = Gloss.color Gloss.blue (Gloss.circleSolid imageSize)
printTile (Tile True Lava False)            = Gloss.color Gloss.red (Gloss.circleSolid imageSize)
printTile (Tile True (Desert False) False)  = Gloss.color (Gloss.light Gloss.orange) (Gloss.circleSolid imageSize)
printTile (Tile True (Desert True) False)   = Gloss.color Gloss.yellow (Gloss.circleSolid imageSize)
printTile (Tile True Portal False)          = Gloss.color Gloss.black (Gloss.circle imageSize)
printTile (Tile True WormBody False)        = Gloss.color Gloss.green (Gloss.circleSolid imageSize)
printTile (Tile True WormHead False)        = Gloss.color Gloss.green (Gloss.circleSolid imageSize)
printTile (Tile True _ True)                = Gloss.color Gloss.black (Gloss.circleSolid imageSize)


printTileAtPos :: Position -> Matrix Tile -> Gloss.Picture
printTileAtPos pos@(x, y) field = Gloss.translate xGloss yGloss (printTile tile)
    where (xGloss, yGloss) = positionToGloss pos
          tile = getTileFromPos pos field

-- Transforme Position@(Int, Int) to Gloss.Position@(Float, Float)
positionToGloss :: Position -> (Float, Float)
positionToGloss pos@(x, y) = (fromIntegral y * tileSize, - fromIntegral x * tileSize)

verificationStateGUI :: GameState -> (Bool, String)
verificationStateGUI gagameState@(player, field, _, _, _, _, _, worms) = do
    if (water player == 0)
        then (False, "You died of thirst !")
    else if (typeTile (getElem (xCoord player) (yCoord player) field) == Lava)
        then (False, "You died in LAVA !")
    else if (typeTile (getElem (xCoord player) (yCoord player) field) == Portal)
        then (False, "Congratz, you find a Portal !")
    else if (typeTile (getElem (xCoord player) (yCoord player) field) == Water)
        then (True, "You find a stock of water")
    else if (typeTile (getElem (xCoord player) (yCoord player) field) == Desert True)
        then (True, "You find a treasure")
    else if (xCoord player, yCoord player) `elem` wormsPositions worms
        then (False, "You got eaten by a worm")
    else (True, "You are alive")
