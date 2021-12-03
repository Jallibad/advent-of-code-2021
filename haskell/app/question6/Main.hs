{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens ((^.), (+~), makeLenses, over, view)
import Control.Monad (when)
import Data.List (uncons)
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V (foldl', fromList, replicate, zipWith)
import Data.Void (Void)
import GHC.TypeLits.Printf (printf)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (ErrorFancy(ErrorFail), MonadParsec(eof, takeWhileP), Parsec, errorBundlePretty, fancyFailure, many, parse)
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

runParser :: Parsec Void String a -> String -> String -> IO a
runParser parser fileName = either (fail . errorBundlePretty) return . parse parser fileName

type BitCriteria = Int -> [Vector Bool] -> [Vector Bool]

main :: IO ()
main = do
    let relativeFileName = "data/question5-input.txt"
    let bitsPerLine = 12
    file <- getDataFileName relativeFileName >>= readFile
    diagnosticLines <- runParser (many (lineParser bitsPerLine) <* eof) relativeFileName file
    let
        countOfOnesAndTotalCount ls index = (countOfOnesAtIndex, totalCount)
            where
                statistics = calculateStatistics bitsPerLine ls
                countOfOnesAtIndex = view counts statistics ! index
                totalCount = statistics ^. totalPoints
        oxygenGeneratorBitCriteria index ls = filter ((== mostCommonValue) . (! index)) ls
            where
                (countOfOnesAtIndex, totalCount) = countOfOnesAndTotalCount ls index
                mostCommonValue = countOfOnesAtIndex * 2 >= totalCount
        co2ScrubberBitCriteria index ls = filter ((== leastCommonValue) . (! index)) ls
            where
                (countOfOnesAtIndex, totalCount) = countOfOnesAndTotalCount ls index
                leastCommonValue = countOfOnesAtIndex * 2 < totalCount

        applyBitCriteria criteria = fmap (binaryToDecimal . fst) . uncons . snd . until isComplete increment . (0,)
            where
                isComplete (_, [_]) = True
                isComplete ((== bitsPerLine) -> True, _) = True
                isComplete _ = False
                increment (i, xs) = (i+1, criteria i xs)
        calculateRating (name :: String) criteria = maybe failure return $ applyBitCriteria criteria diagnosticLines
            where failure = fail $ printf @"All lines removed when calculating %s rating" name
    oxygenGeneratorRating <- calculateRating "oxygen generator" oxygenGeneratorBitCriteria
    co2ScrubberRating <- calculateRating "CO2 scrubber" co2ScrubberBitCriteria
    print $ oxygenGeneratorRating * co2ScrubberRating
