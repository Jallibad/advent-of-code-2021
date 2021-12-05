{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens ((^.), (+~), makeLenses, over)
import Control.Lens.Cons (_Cons)
import Control.Lens.Tuple
import Data.Array
import Data.Void (Void)
import GHC.TypeLits.Printf (printf)
import Parsing (runParser)
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

size :: (Int, Int)
size = (5, 5)

data BingoCell = BingoCell {_isFilled :: !Bool, _number :: !Int}
$(makeLenses ''BingoCell)

fillIfCalled :: Int -> BingoCell -> BingoCell
fillIfCalled calledNumber (BingoCell False cellNumber) = BingoCell (calledNumber == cellNumber) cellNumber
fillIfCalled _ cell = cell

instance Show BingoCell where
    show (BingoCell filled n) = printf @"%c%-.2d" (if filled then 'F' else ' ') n

newtype BingoBoard = BingoBoard (Array (Int, Int) BingoCell)
    deriving newtype Show

fillCells :: Int -> BingoBoard -> BingoBoard
fillCells calledNumber (BingoBoard board) = BingoBoard $ fillIfCalled calledNumber <$> board

boardParser :: Parsec Void String BingoBoard
boardParser = do
    let totalCells = size ^. _1 * size ^. _2
    cells <- count totalCells $ decimal <* space1
    return $ BingoBoard $ listArray ((1,1), size) $ BingoCell False <$> cells

data BingoGame = BingoGame {_numbers :: ![Int], _boards :: ![BingoBoard]}
    deriving stock Show
$(makeLenses ''BingoGame)

callNumber :: BingoBoard -> BingoBoard
callNumber board = undefined
    where numberToCall = board ^. numbers . _Cons

gameParser :: Parsec Void String BingoGame
gameParser = do
    parsedNumbers <- many $ decimal <* optional (string ",")
    many space1
    parsedBoards <- many boardParser
    return $ BingoGame parsedNumbers parsedBoards

main :: IO ()
main = do
    let relativeFileName = "question7-example-input.txt"
    file <- readFile "C:\\Users\\Jordan\\Documents\\Programming\\advent-of-code-2021\\haskell\\data\\question7-example-input.txt"
    game <- runParser gameParser relativeFileName file
    print game
