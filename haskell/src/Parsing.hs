module Parsing where

import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)

runParser :: Parsec Void String a -> String -> String -> IO a
runParser parser fileName = either (fail . errorBundlePretty) return . parse parser fileName
