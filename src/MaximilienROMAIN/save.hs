-- Maximilien Romain
-- 0543411

module Save where

import Player
import Fields
import Worm

import Data.List
import Data.Matrix
import System.Environment
import System.Random


saveGame :: Options -> Player -> Matrix Tile -> [Worm] -> IO ()
saveGame options player field worms = writeFile "saveGame.txt" $ gameOptionsToString options player field worms

gameOptionsToString :: Options -> Player -> Matrix Tile -> [Worm] -> String
gameOptionsToString (s, m, g, t, w, p, l, ll, x, y) player field worms = (intercalate "\n" $ [ renderPlayer player,
       "s " ++ renderInt s,
       "m " ++ renderInt m,
       "g " ++ renderInt g,
       "t " ++ renderDouble t,
       "w " ++  renderDouble w,
       "p " ++ renderDouble p,
       "l " ++ renderDouble l,
       "ll " ++ renderDouble ll,
       "x " ++ renderInt x,
       "y " ++ renderInt y]) ++ "\n" ++ reveleadString ++ "\n" ++ wormsString
       where reveleadString = intercalate "\n" $ map writeRevealed $ getRevealedTiles field
             wormsString = intercalate "\n" $ map wormToString worms

renderInt :: Int -> String
renderInt a = concat ["( ", show a, " )"]

renderDouble :: Double -> String
renderDouble a = concat ["( ", show (round a :: Int), " )"]

renderPosition :: Position -> String
renderPosition (x, y) = concat ["( ", show x, ", ", show y, " )" ]

renderPlayer :: Player -> String
renderPlayer player = intercalate "\n" $
                        [ "player " ++ renderPosition (xCoord player, yCoord player),
                          "water " ++ renderInt (water player)]

-- Return a list of revealed tiles from the game field
getRevealedTiles :: Matrix Tile -> [Position]
getRevealedTiles field = searchRevealed (nrows field) (ncols field) field []
    where searchRevealed 1 1 field xs
            | isRevealed (field!(1,1)) == True = xs ++ [(1,1)]
            | otherwise = xs
          searchRevealed x 1 field xs
            | isRevealed (field!(x,1)) == True = searchRevealed (x - 1) (ncols field) field (xs ++ [(x,1)])
            | otherwise = searchRevealed (x - 1) (ncols field) field xs
          searchRevealed x y field xs
            | isRevealed (field!(x,y)) == True = searchRevealed x (y - 1) field (xs ++ [(x,y)])
            | otherwise = searchRevealed x (y - 1) field xs

writeRevealed :: Position -> String
writeRevealed (x,y) = concat ["revealed ( ", show x, " , ", show y, " )"]

wormToString :: Worm -> String
wormToString worm
    | state worm == True =  let positions = wormSequence worm
                            in "emerging ( " ++ intercalate " , " (map renderPosition positions) ++ " )"
    | state worm == False = let positions = wormSequence worm
                            in "disappearing ( " ++ intercalate "," (map renderPosition positions) ++ " )"
