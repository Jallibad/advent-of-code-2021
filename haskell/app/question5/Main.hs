{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens ((^.), (+~), makeLenses, over)
import Control.Monad (when)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V (foldl', fromList, map, replicate, zipWith)
import Data.Void (Void)
import GHC.TypeLits.Printf (printf)
import Parsing (runParser)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (fancyFailure, many, Parsec, MonadParsec(eof, takeWhileP), ErrorFancy(ErrorFail))
import Text.Megaparsec.Char (space1)

data Statistics = Statistics {_totalPoints :: !Int, _counts :: !(Vector Int)}
$(makeLenses ''Statistics)

addStatistic :: Vector Bool -> Statistics -> Statistics
addStatistic line = over counts (combine line) . (totalPoints +~ 1)

combine :: Vector Bool -> Vector Int -> Vector Int
combine = flip $ V.zipWith (\c b -> if b then c + 1 else c)

calculateStatistics :: Foldable f => Int -> f (Vector Bool) -> Statistics
calculateStatistics = foldr addStatistic . Statistics 0 . flip V.replicate 0

lineParser :: Int -> Parsec Void String (Vector Bool)
lineParser expectedLineLength = do
    line <- takeWhileP Nothing $ flip (elem @[]) ['0','1']
    let parsedLineLength = length line
    when (parsedLineLength /= expectedLineLength) $
        fancyFailure [ErrorFail $ printf @"expected line of length %d but got %d" expectedLineLength parsedLineLength]
    _ <- many space1
    return $ V.fromList $ (== '1') <$> line

binaryToDecimal :: Vector Bool -> Int
binaryToDecimal = V.foldl' (\t b -> t * 2 + if b then 1 else 0) 0

gamma :: Statistics -> Int
gamma statistics = binaryToDecimal $ V.map ((> total) . (* 2)) $ statistics ^. counts
    where total = statistics ^. totalPoints

epsilon :: Statistics -> Int
epsilon statistics = binaryToDecimal $ V.map ((< total) . (* 2)) $ statistics ^. counts
    where total = statistics ^. totalPoints

main :: IO ()
main = do
    let relativeFileName = "data/question5-input.txt"
    let bitsPerLine = 12
    file <- getDataFileName relativeFileName >>= readFile
    diagnosticLines <- runParser (many (lineParser bitsPerLine) <* eof) relativeFileName file
    let statistic = calculateStatistics bitsPerLine diagnosticLines
    print $ gamma statistic * epsilon statistic
