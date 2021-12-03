{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens ((^.), (+~), makeLenses, over)
import Control.Monad (when)
import Data.Vector.Unboxed (Vector, foldl', fromList, map, replicate, zipWith)
import Data.Void (Void)
import GHC.TypeLits.Printf (printf)
import Paths_advent_of_code (getDataFileName)
import Prelude hiding (map, replicate, zipWith)
import Text.Megaparsec (fancyFailure, parse, errorBundlePretty, many, Parsec, MonadParsec(eof, takeWhileP), ErrorFancy(ErrorFail))
import Text.Megaparsec.Char (space1)

data Statistics = Statistics {_totalPoints :: !Int, _counts :: !(Vector Int)}
    deriving stock Show
$(makeLenses ''Statistics)

emptyStatistics :: Int -> Statistics
emptyStatistics = Statistics 0 . flip replicate 0

addStatistic :: Vector Bool -> Statistics -> Statistics
addStatistic line = over counts (combine line) . (totalPoints +~ 1)

combine :: Vector Bool -> Vector Int -> Vector Int
combine = flip $ zipWith (\c b -> if b then c + 1 else c)

lineParser :: Int -> Parsec Void String (Vector Bool)
lineParser expectedLineLength = do
    line <- takeWhileP Nothing $ flip (elem @[]) ['0','1']
    let parsedLineLength = length line
    when (parsedLineLength /= expectedLineLength) $
        fancyFailure [ErrorFail $ printf @"expected line of length %d but got %d" expectedLineLength parsedLineLength]
    _ <- many space1
    return $ fromList $ (== '1') <$> line

binaryToDecimal :: Vector Bool -> Int
binaryToDecimal = foldl' (\t b -> t * 2 + if b then 1 else 0) 0

gamma :: Statistics -> Int
gamma statistics = binaryToDecimal $ map ((> total) . (* 2)) $ statistics ^. counts
    where total = statistics ^. totalPoints

epsilon :: Statistics -> Int
epsilon statistics = binaryToDecimal $ map ((< total) . (* 2)) $ statistics ^. counts
    where total = statistics ^. totalPoints

runParser :: Parsec Void String a -> String -> String -> IO a
runParser parser fileName = either (fail . errorBundlePretty) return . parse parser fileName

main :: IO ()
main = do
    let relativeFileName = "data/question5-input.txt"
    let bitsPerLine = 12
    file <- getDataFileName relativeFileName >>= readFile
    diagnosticLines <- runParser (many (lineParser bitsPerLine) <* eof) relativeFileName file
    let statistic = foldr addStatistic (emptyStatistics bitsPerLine) diagnosticLines
    print $ gamma statistic * epsilon statistic
