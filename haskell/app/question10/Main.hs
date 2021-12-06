{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative (Applicative(liftA2))
import Control.Lens ((^.), makeLenses)
import Data.Map (Map)
import qualified Data.Map as Map (filter, fromDistinctAscList, size, unionsWith)
import Data.Set (Set)
import qualified Data.Set as Set (fromAscList, fromList, map, toAscList)
import Data.Void (Void)
import GHC.TypeLits.Printf (printf)
import Parsing (runParser)
import Paths_advent_of_code (getDataFileName)
import Text.Megaparsec (MonadParsec(eof), Parsec, many)
import Text.Megaparsec.Char (space1, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Point = Point {_x :: Int, _y :: Int}
    deriving stock Eq
    deriving stock Ord
$(makeLenses ''Point)

instance Show Point where
    show point = printf @"%d,%d" (point ^. x) (point ^. y)

pointParser :: Parsec Void String Point
pointParser = liftA2 Point (decimal <* string ",") decimal <* many space1

data LineSegment = LineSegment {_point1 :: Point, _point2 :: Point}
$(makeLenses ''LineSegment)

instance Show LineSegment where
    show lineSegment = printf @"%s -> %s" (show $ lineSegment ^. point1) (show $ lineSegment ^. point2)

allPoints :: LineSegment -> Set Point
allPoints (LineSegment (Point x1 y1) (Point x2 y2))
    | x1 == x2 = Set.map (Point x1) $ Set.fromAscList yCast
    | y1 == y2 = Set.map (`Point` y1) $ Set.fromAscList xCast
    | abs (x1 - x2) == abs (y1 - y2) = Set.fromList $ zipWith Point (if x1 > x2 then xCast else reverse xCast) (if y1 > y2 then yCast else reverse yCast)
    | otherwise = []
    where
        xCast = [min x1 x2.. max x1 x2]
        yCast = [min y1 y2.. max y1 y2]

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
