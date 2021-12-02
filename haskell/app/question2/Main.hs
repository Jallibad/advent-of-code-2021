module Main where

import Paths_advent_of_code (getDataFileName)

exampleInput :: [Int]
exampleInput = [199,200,208,210,200,207,240,269,260,263]

slidingSums :: Num a => [a] -> [a]
slidingSums xs = zipWith3 (\a b c -> a + b + c) xs (tail xs) (drop 2 xs)

countIncreases :: Ord a => [a] -> Int
countIncreases xs = length $ filter id $ zipWith (<) xs $ tail xs

main :: IO ()
main = do
    file <- getDataFileName "data/question1-input.txt" >>= readFile
    print $ countIncreases $ slidingSums $ read @Int <$> lines file
