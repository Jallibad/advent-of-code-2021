module Main where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Data.Void (Void)
import Parsing (runParser)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (Parsec, many, optional)
import Text.Megaparsec.Char (space1, string)
import Text.Megaparsec.Char.Lexer (decimal)

exampleInput :: [Int]
exampleInput = [16,1,2,0,4,2,7,1,2,14]

exampleInputMap :: IntMap Int
exampleInputMap = fromPositionsList exampleInput

fromPositionsList :: [Int] -> IntMap Int
fromPositionsList = IntMap.fromListWith (+) . fmap (,1)

fuelUsage :: IntMap Int -> Int -> Int
fuelUsage crabPositions finalPosition = IntMap.foldlWithKey foldFunction 0 crabPositions
    where foldFunction runningTotal position count = runningTotal + abs (position - finalPosition) * count

possiblePositions :: IntMap Int -> [Int]
possiblePositions crabPositions = do
    (minimalPosition, _) <- maybeToList $ IntMap.lookupMin crabPositions
    (maximalPosition, _) <- maybeToList $ IntMap.lookupMax crabPositions
    [minimalPosition.. maximalPosition]

minimumFuelCost :: IntMap Int -> Int
minimumFuelCost crabPositions = minimum $ fuelUsage crabPositions <$> possiblePositions crabPositions

crabPositionsParser :: Parsec Void String (IntMap Int)
crabPositionsParser = fromPositionsList <$> many (decimal <* optional (string ",")) <* many space1

main :: IO ()
main = do
    let relativeFileName = "data/question13-input.txt"
    file <- getDataFileName relativeFileName >>= readFile
    crabPositions <- runParser crabPositionsParser relativeFileName file
    print $ minimumFuelCost crabPositions
