module Utils where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec ( some, Parsec )
import Text.Megaparsec.Char ( digitChar )

readIntsFromFile :: FilePath -> IO [Int]
readIntsFromFile = readLinesFromFile read

readLinesFromFile :: (String -> a) -> FilePath -> IO [a]
readLinesFromFile lineParseFunction filepath =
    map lineParseFunction . lines <$> readFile filepath

countWhere :: (a -> Bool) -> [a] -> Int
countWhere predicate list = length $ filter predicate list

parsePositiveNumber :: Parsec Void Text Int
parsePositiveNumber = read <$> some digitChar
