{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens ((^.), (+~), (-~), makeLenses)
import Data.Foldable (Foldable (foldl'))
import Data.Void (Void)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (MonadParsec (eof), Parsec, (<|>), errorBundlePretty, many, parse)
import Text.Megaparsec.Char (space1, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Position = Position {_horizontalPosition :: Int, _depth :: Int, _aim :: Int}
$(makeLenses ''Position)

data Course = Forward Int | Down Int | Up Int

courseParser :: Parsec Void String Course
courseParser = helper "forward" Forward <|> helper "down" Down <|> helper "up" Up
    where helper name constructor = string name >> many space1 >> (constructor <$> decimal) <* many space1

navigate :: Position -> Course -> Position
navigate position (Forward distance) = (depth +~ (position ^. aim * distance)) $ (horizontalPosition +~ distance) position
navigate position (Down distance) = (aim +~ distance) position
navigate position (Up distance) = (aim -~ distance) position

main :: IO ()
main = do
    let relativeFileName = "data/question3-input.txt"
    file <- getDataFileName relativeFileName >>= readFile
    case parse (many courseParser <* eof) relativeFileName file of
        Left parseError -> putStrLn $ errorBundlePretty parseError
        Right courses -> let Position h d _ = foldl' navigate (Position 0 0 0) courses in print $ h * d
