module Utility where

import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V (foldl')

binaryToDecimalV :: Vector v Bool => v Bool -> Int
binaryToDecimalV = V.foldl' (\t b -> t * 2 + if b then 1 else 0) 0
