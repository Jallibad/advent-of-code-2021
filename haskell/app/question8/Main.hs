{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens (Each(each), Fold, (%=), (.=), (^.), allOf, filtered, folding, makeLenses, mapped, productOf, sumOf, use)
import Control.Monad.State.Lazy (MonadState, runState)
import Data.Array (Array, (!), elems, listArray)
import Data.List ((\\), uncons)
import Data.Void (Void)
import GHC.TypeLits.Printf (printf)
import Parsing (runParser)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (Parsec, count, many, optional)
import Text.Megaparsec.Char (space1, string)
import Text.Megaparsec.Char.Lexer (decimal)

size :: (Int, Int)
size = (5, 5)

data BingoCell = BingoCell {_isFilled :: !Bool, _number :: !Int}
    deriving stock Eq
$(makeLenses ''BingoCell)

fillIfCalled :: Int -> BingoCell -> BingoCell
fillIfCalled calledNumber (BingoCell False cellNumber) = BingoCell (calledNumber == cellNumber) cellNumber
fillIfCalled _ cell = cell

instance Show BingoCell where
    show (BingoCell filled n) = printf @"%c%-.2d" (if filled then 'F' else ' ') n

newtype BingoBoard = BingoBoard (Array (Int, Int) BingoCell)
    deriving newtype Eq
    deriving newtype Show

allCells :: Fold BingoBoard BingoCell
allCells = folding $ \(BingoBoard board) -> elems board

fillCells :: Int -> BingoBoard -> BingoBoard
fillCells calledNumber (BingoBoard board) = BingoBoard $ fillIfCalled calledNumber <$> board

row :: Int -> Fold BingoBoard BingoCell
row columnIndex = folding $ \(BingoBoard b) -> (b !) <$> rowIndices
    where rowIndices :: [(Int, Int)] = (,columnIndex) <$> [1.. fst size]

column :: Int -> Fold BingoBoard BingoCell
column rowIndex = folding $ \(BingoBoard b) -> (b !) <$> columnIndices
    where columnIndices :: [(Int, Int)] = (rowIndex,) <$> [1.. snd size]

boardIsComplete :: BingoBoard -> Bool
boardIsComplete b = any isFilledForGetter $ (row <$> [1.. fst size]) ++ (column <$> [1.. snd size])
    where isFilledForGetter getter = allOf getter (^. isFilled) b

sumOfUnfilledCells :: BingoBoard -> Int
sumOfUnfilledCells = sumOf $ allCells . filtered (not . _isFilled) . number

boardParser :: Parsec Void String BingoBoard
boardParser = do
    let totalCells = productOf each size
    cells <- count totalCells $ decimal <* space1
    return $ BingoBoard $ listArray ((1,1), size) $ BingoCell False <$> cells

firstCell :: BingoBoard -> BingoCell
firstCell (BingoBoard board) = board ! (1,1)

data BingoGame = BingoGame {_numbers :: ![Int], _boards :: ![BingoBoard]}
    deriving stock Show
$(makeLenses ''BingoGame)

callNumber :: MonadState BingoGame m => m (Maybe Int, [BingoBoard])
callNumber = use numbers >>= maybe (return (Nothing, [])) applyCalledNumber . uncons
    where applyCalledNumber (numberToCall, remainingNumbers) = do
                numbers .= remainingNumbers
                alreadyCompletedBoards <- filter boardIsComplete <$> use boards
                boards . mapped . filtered (not . boardIsComplete) %= fillCells numberToCall
                finallyCompletedBoards <- filter boardIsComplete <$> use boards
                return (Just numberToCall, finallyCompletedBoards \\ alreadyCompletedBoards)

gameIsComplete :: BingoGame -> Bool
gameIsComplete game = all boardIsComplete $ game ^. boards

runGameUntilComplete :: BingoGame -> ((Maybe Int, [BingoBoard]), BingoGame)
runGameUntilComplete = until (gameIsComplete . snd) (runState callNumber . snd) . ((Nothing, []),)

gameParser :: Parsec Void String BingoGame
gameParser = do
    parsedNumbers <- many $ decimal <* optional (string ",")
    many space1
    parsedBoards <- many boardParser
    return $ BingoGame parsedNumbers parsedBoards

main :: IO ()
main = do
    let relativeFileName = "data/question7-input.txt"
    file <- getDataFileName relativeFileName >>= readFile
    game <- runParser gameParser relativeFileName file
    let ((Just lastCalledNumber, [lastCompletedBoard]), _) = runGameUntilComplete game
    print $ sumOfUnfilledCells lastCompletedBoard * lastCalledNumber
