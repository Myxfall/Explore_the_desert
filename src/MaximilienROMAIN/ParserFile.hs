-- Maximilien Romain
-- 0543411

module ParserFile where

import Parser
import Control.Monad
import Data.Map.Strict

import Fields
import Player


type MapInt = Map String Int
type MapPosition = Map String [Position]
type MapWorms = Map String [[Position]]

-- Type of Line for parser
data GameLine =  GamePosition Position  | GameSupply Int            | GameRevealed Position
                | GameCollected Position| GameEmerging [Position]   | GameDisappearing [Position]
                | GameS Int             | GameM Int                 | GameG Int
                | GameT Int             | GameW Int                 | GameP Int
                | GameL Int             | GameLl Int                | GameX Int
                | GameY Int
                deriving Show

parenthesis :: Parser.Parser a -> Parser.Parser a
parenthesis p = do Parser.keyword "("
                   x <- p
                   Parser.keyword ")"
                   return x

-- ##### Loading #####
-- Retrieving Naturals
parserToNatural :: GameLine -> MapInt -> MapInt
parserToNatural (GameSupply line) sMap = insert "water" line sMap
parserToNatural (GameS line)  sMap = insert "s"  line sMap
parserToNatural (GameM line)  sMap = insert "m"  line sMap
parserToNatural (GameG line)  sMap = insert "g"  line sMap
parserToNatural (GameT line)  sMap = insert "t"  line sMap
parserToNatural (GameW line)  sMap = insert "w"  line sMap
parserToNatural (GameP line)  sMap = insert "p"  line sMap
parserToNatural (GameL line)  sMap = insert "l"  line sMap
parserToNatural (GameLl line) sMap = insert "ll" line sMap
parserToNatural (GameX line)  sMap = insert "x"  line sMap
parserToNatural (GameY line)  sMap = insert "y"  line sMap
parserToNatural _ sMap = insert "noFound" 0 sMap

-- Retrieving Positions
parserToPositions :: GameLine -> MapPosition -> MapPosition
parserToPositions (GamePosition line) sMap = insert "player" [line] sMap
parserToPositions (GameRevealed line) sMap = insertWith (++) "revealed" [line] sMap
parserToPositions (GameCollected line) sMap = insertWith (++) "collected" [line] sMap
parserToPositions _ sMap = insert "noFound" [] sMap

parserToWorms :: GameLine -> MapWorms -> MapWorms
parserToWorms (GameEmerging line) sMap = insertWith (++) "emerging" [line] sMap
parserToWorms (GameDisappearing line) sMap = insertWith (++) "disappearing" [line] sMap
parserToWorms _ sMap = insert "noFound" [] sMap

extractOptions :: [GameLine] -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
extractOptions parsing = options
                         where naturalMap = Prelude.foldr parserToNatural empty parsing
                               options = (  naturalMap ! "water", naturalMap ! "s", naturalMap ! "m",
                                            naturalMap ! "g", naturalMap ! "t", naturalMap ! "w",
                                            naturalMap ! "p", naturalMap ! "l", naturalMap ! "ll",
                                            naturalMap ! "x", naturalMap ! "y" )

extractPosition :: [GameLine] -> [[Position]]
extractPosition parsing = positions
                           where positionsMaps = Prelude.foldr parserToPositions empty parsing
                                 positions = [findWithDefault [] "player" positionsMaps] ++ [findWithDefault [] "revealed" positionsMaps] ++ [findWithDefault [] "collected" positionsMaps]

extractWorms :: [GameLine] -> ([[Position]], [[Position]])
extractWorms parsing = worms
    where wormsPositions = Prelude.foldr parserToWorms empty parsing
          worms@(a, b) =  (findWithDefault [] "emerging" wormsPositions, findWithDefault [] "disappearing" wormsPositions)


-- ##### Parsing #####
parse :: String -> [GameLine]
parse str = if not (Prelude.null parsed)
            then fst (head parsed)
            else []
            where parsed = Parser.apply parseGame str

parseLine :: Parser.Parser GameLine
parseLine = Parser.oneof [parseStringSupply, parseStringPosition, parseStringRevealed,
                            parseStringCollected, parseStringEmerging, parseStringDisappearing,
                            parseStringS, parseStringM, parseStringG, parseStringT, parseStringW,
                            parseStringP, parseStringL, parseStringLl, parseStringX, parseStringY]

parseGame :: Parser.Parser [GameLine]
parseGame = do l <- parseLine
               ls <- Parser.many (Parser.string "\n" >> parseLine)
               return (l:ls)

parseNatural :: Parser.Parser Int
parseNatural = do Parser.fromRead

parseStringPosition :: Parser.Parser GameLine
parseStringPosition = Parser.keyword "player" >> parenthesis (Control.Monad.liftM GamePosition parsePosition)

parsePosition :: Parser.Parser Position
parsePosition = do x <- parseNatural
                   Parser.keyword ","
                   y <- parseNatural
                   return (x, y)

parseWorm :: Parser.Parser Position
parseWorm = parenthesis (do  x <- parseNatural
                             Parser.keyword ","
                             y <- parseNatural
                             return (x, y))

-- Parser for emerging and disappearing worms
parsePositions :: Parser.Parser [Position]
parsePositions = do parsed <- parseWorm
                    rest <- Parser.many (Parser.keyword "," >> parseWorm)
                    return (parsed:rest)

parseStringSupply :: Parser.Parser GameLine
parseStringSupply = Parser.keyword "water" >> parenthesis (Control.Monad.liftM GameSupply parseNatural)


parseStringRevealed :: Parser.Parser GameLine
parseStringRevealed = Parser.keyword "revealed" >> parenthesis (Control.Monad.liftM GameRevealed parsePosition)

parseStringCollected :: Parser.Parser GameLine
parseStringCollected = Parser.keyword "collected" >> parenthesis (Control.Monad.liftM GameCollected parsePosition)

parseStringEmerging :: Parser.Parser GameLine
parseStringEmerging = do Parser.keyword "emerging"
                         p <- parenthesis parsePositions
                         return (GameEmerging p)

parseStringDisappearing :: Parser.Parser GameLine
parseStringDisappearing = do Parser.keyword "disappearing"
                             p <- parenthesis parsePositions
                             return (GameDisappearing p)

parseStringS :: Parser.Parser GameLine
parseStringS = Parser.keyword "s" >> parenthesis (Control.Monad.liftM GameS parseNatural)

parseStringM :: Parser.Parser GameLine
parseStringM = Parser.keyword "m" >> parenthesis (Control.Monad.liftM GameM parseNatural)

parseStringG :: Parser.Parser GameLine
parseStringG = Parser.keyword "g" >> parenthesis (Control.Monad.liftM GameG parseNatural)

parseStringT :: Parser.Parser GameLine
parseStringT = Parser.keyword "t" >> parenthesis (Control.Monad.liftM GameT parseNatural)

parseStringW :: Parser.Parser GameLine
parseStringW = Parser.keyword "w" >> parenthesis (Control.Monad.liftM GameW parseNatural)

parseStringP :: Parser.Parser GameLine
parseStringP = Parser.keyword "p" >> parenthesis (Control.Monad.liftM GameP parseNatural)

parseStringL :: Parser.Parser GameLine
parseStringL = Parser.keyword "l" >> parenthesis (Control.Monad.liftM GameL parseNatural)

parseStringLl :: Parser.Parser GameLine
parseStringLl = Parser.keyword "ll" >> parenthesis (Control.Monad.liftM GameLl parseNatural)

parseStringX :: Parser.Parser GameLine
parseStringX = Parser.keyword "x" >> parenthesis (Control.Monad.liftM GameX parseNatural)

parseStringY :: Parser.Parser GameLine
parseStringY = Parser.keyword "y" >> parenthesis (Control.Monad.liftM GameY parseNatural)
