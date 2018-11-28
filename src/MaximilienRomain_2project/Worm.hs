-- Maximilien Romain
-- 0543411

module Worm where

import Player

data Worm = Worm { beginWorm :: Position,   -- Worm's head
                    endWorm :: Position,    -- Worm's tale
                    direction :: Position,  -- Worm's direction (i.e. (0,1)) right direction
                    lengthWorm :: Int,      -- Worm's lenght
                    state :: Bool}     deriving Show     -- True emerging | False disappearing

-- Return a new emerging Worm
newWorm :: Position -> Position -> Int -> Worm
newWorm pos direction lenght = Worm pos pos direction lenght True

wormBeginPos :: Worm -> Position
wormBeginPos worm = beginWorm worm

wormEndPos :: Worm -> Position
wormEndPos worm = endWorm worm

wormDirection :: Worm -> Position
wormDirection worm = direction worm

wormLife :: Worm -> Int
wormLife worm = lengthWorm worm

wormState :: Worm -> Bool
wormState worm = state worm

updateState :: Worm -> Worm
updateState (Worm start end dir l state) = Worm start end dir l False

-- Return list of positions between start and end
wormSequence :: Worm -> [Position]
wormSequence worm = loop [start]
    where   start@(x1, y1) = beginWorm worm
            end@(x2, y2)  = endWorm worm
            dir@(x,y) = direction worm
            loop seqs
                | l == x2 && k == y2 = seqs
                | x1 == x2 = loop (seqs ++ [(x1, snd (last seqs) + y)])
                | y1 == y2 = loop (seqs ++ [(fst (last seqs) + x, y1)])
                where (l,k) = last seqs

-- return next move of worm
wormNextMove :: Worm -> Position
wormNextMove worm = (x + xDir, y + yDir)
    where   (x,y) = endWorm worm
            (xDir, yDir) = direction worm

canStillMove :: Worm -> Bool
canStillMove worm = bool
    where wormPos = wormSequence worm
          wormLenght = lengthWorm worm
          bool = not ( wormLenght == (length wormPos))

-- Update the worm. The bool indicate if the worm can still move or must disappear
wormMovement :: Worm -> Bool -> Worm
wormMovement worm canMove = updatedWorm
    where   updatedWorm = if canMove && state worm
                            then let upWorm = wormEmergingMovement worm
                                 in if canStillMove upWorm
                                     then upWorm
                                     else updateState upWorm
                            else disappearingWormMovement (updateState worm)

wormEmergingMovement :: Worm -> Worm
wormEmergingMovement (Worm start endPos@(x,y) dir@(x1,y1) l state) = Worm start (x+x1, y+y1) dir l state

disappearingWormMovement :: Worm -> Worm
disappearingWormMovement (Worm start@(x,y) end dir@(x1,y1) l state) = Worm (x+x1, y+y1) end dir l state

-- Remove worms with over lifespan
cleanWorms :: [Worm] -> [Worm]
cleanWorms worms = filter (\worm -> (beginWorm worm /= endWorm worm) || (state worm == True)) worms

isStillAlive :: Worm -> Bool
isStillAlive worm = (beginWorm worm /= endWorm worm) || (state worm == True)

wormsPositions :: [Worm] -> [Position]
wormsPositions worms = concat $ map wormSequence worms

createWormsFromPositions :: [[Position]] -> Bool -> Int -> [Worm]
createWormsFromPositions pos state lifespan = map (\p -> deducteWormFromPositions p state lifespan) pos

deducteWormFromPositions :: [Position] -> Bool -> Int -> Worm
deducteWormFromPositions pos state lifespan = worm
    where   firstPos@(x1 , y1) = head pos
            lastPos@(x2 , y2) = last pos
            guessDirection
                | x1 == x2 && y1 <= y2 = (0, 1)
                | x1 == x2 && y1 > y2 = (0, -1)
                | y1 == y2 && x1 <= x2 = (1, 0)
                | y1 == y2 && x1 > x2 = (-1, 0)
            worm = Worm firstPos lastPos guessDirection lifespan state
