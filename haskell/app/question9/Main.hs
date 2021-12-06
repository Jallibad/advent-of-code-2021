{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative (Applicative(liftA2))
import Control.Lens ((^.), makeLenses)
import Data.Map (Map)
import qualified Data.Map as Map (filter, fromDistinctAscList, size, unionsWith)
import Data.Set (Set)
import qualified Data.Set as Set (map, toAscList)
import Data.Void (Void)
import GHC.TypeLits.Printf (printf)
import Parsing (runParser)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (MonadParsec(eof), Parsec, many)
import Text.Megaparsec.Char (space1, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Point = Point {_x :: !Int, _y :: !Int}
    deriving stock Eq
    deriving stock Ord
$(makeLenses ''Point)

instance Show Point where
    show point = printf @"%d,%d" (point ^. x) (point ^. y)

pointParser :: Parsec Void String Point
pointParser = liftA2 Point (decimal <* string ",") decimal <* many space1

data LineSegment = LineSegment {_point1 :: !Point, _point2 :: !Point}
$(makeLenses ''LineSegment)

instance Show LineSegment where
    show lineSegment = printf @"%s -> %s" (show $ lineSegment ^. point1) (show $ lineSegment ^. point2)

allPoints :: LineSegment -> Set Point
allPoints (LineSegment (Point x1 y1) (Point x2 y2))
    | x1 == x2 = yCast
    | y1 == y2 = xCast
    | otherwise = []
    where
        xCast = Set.map (`Point` y1) [min x1 x2.. max x1 x2]
        yCast = Set.map (Point x1) [min y1 y2.. max y1 y2]

lineSegmentParser :: Parsec Void String LineSegment
lineSegmentParser = liftA2 LineSegment (pointParser <* string "->" <* many space1) pointParser <* many space1

countOverlaps :: (Foldable f, Functor f) => f LineSegment -> Map Point Int
countOverlaps = Map.unionsWith (+) . fmap thing
    where thing = Map.fromDistinctAscList . fmap (,1) . Set.toAscList . allPoints

main :: IO ()
main = do
    let relativeFileName = "data/question9-input.txt"
    file <- getDataFileName relativeFileName >>= readFile
    lineSegments <- runParser (many lineSegmentParser <* eof) relativeFileName file
    print $ Map.size $ Map.filter (>=2) $ countOverlaps lineSegments
