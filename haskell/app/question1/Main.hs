module Main where

import Paths_advent_of_code (getDataFileName)

countIncreases :: Ord a => [a] -> Int
countIncreases xs = length $ filter id $ zipWith (<) xs $ tail xs

main :: IO ()
main = do
    file <- getDataFileName "data/question1-input.txt" >>= readFile
    print $ countIncreases $ read @Int <$> lines file
