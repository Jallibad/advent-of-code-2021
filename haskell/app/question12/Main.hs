{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens (Ixed(ix), folded, sumOf, use, (%=), (+=), makeLenses)
import Control.Monad.State.Strict (execStateT)
import Data.Foldable (Foldable(foldl'))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq((:|>), (:<|)))
import qualified Data.Sequence as Seq
import Data.Void (Void)
import Parsing (runParser)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (MonadParsec(eof), Parsec, many, optional)
import Text.Megaparsec.Char (space1, string)
import Text.Megaparsec.Char.Lexer (decimal)

daysBetweenSpawning :: Int
daysBetweenSpawning = 7

daysToMature :: Int
daysToMature = 2

newtype Ocean = Ocean {_lanternFishCountByTimeToSpawn :: Seq Integer}
    deriving newtype Show
$(makeLenses ''Ocean)

oceanParser :: Parsec Void String Ocean
oceanParser = do
    let startingMap = Map.fromDistinctAscList $ (,0) <$> [0.. daysBetweenSpawning + daysToMature - 1]
    lanternFish <- many (lanternFishParser <* optional (string ",")) <* many space1
    return $ Ocean $ Seq.fromList $ Map.elems $ foldl' (Map.unionWith (+)) startingMap lanternFish

lanternFishParser :: Parsec Void String (Map Int Integer)
lanternFishParser = flip Map.singleton 1 <$> decimal

nextDay :: Ocean -> Maybe Ocean
nextDay = execStateT $ do
    spawning :<| _ <- use lanternFishCountByTimeToSpawn
    lanternFishCountByTimeToSpawn . ix daysBetweenSpawning += spawning
    lanternFishCountByTimeToSpawn %= (:|> spawning) . Seq.drop 1

runForDays :: Int -> Ocean -> Maybe Integer
runForDays days ocean = sumOf (lanternFishCountByTimeToSpawn . folded) <$> oceanAfterDays
    where oceanAfterDays = iterate (>>= nextDay) (Just ocean) !! days

main :: IO ()
main = do
    let relativeFileName = "data/question11-input.txt"
    file <- getDataFileName relativeFileName >>= readFile
    ocean <- runParser (oceanParser <* eof) relativeFileName file
    maybe (fail "Somehow the ocean ended up with fish in the wrong state") print $ runForDays 256 ocean
