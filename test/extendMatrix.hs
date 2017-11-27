import Data.Matrix

data Weapon = Rock | Paper | Scissor deriving (Show)
mat = [Rock, Paper, Scissor, Rock]

matB = fromList 2 2 mat

main :: IO ()
main = do
    putStrLn $ show $ matB
    let newMat = extendTo 3 3 matB
    putStrLn $ show $ newMat
