module Utils where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec ( some, Parsec )
import Text.Megaparsec.Char ( digitChar )

-- Reading From Files
readIntsFromFile :: FilePath -> IO [Int]
readIntsFromFile = readLinesFromFile read

readStringsFromFile :: FilePath -> IO [String]
readStringsFromFile = readLinesFromFile id

readLinesFromFile :: (String -> a) -> FilePath -> IO [a]
readLinesFromFile lineParseFunction filepath =
    map lineParseFunction . lines <$> readFile filepath

-- Parsers
parsePositiveNumber :: Parsec Void Text Int
parsePositiveNumber = read <$> some digitChar

-- Solution Patterns
countWhere :: (a -> Bool) -> [a] -> Int
countWhere predicate list = length $ filter predicate list

-- Common Structures
type OccMap a = Map a Word

incKey :: (Ord a) => OccMap a -> a -> OccMap a
incKey prevMap key = case M.lookup key prevMap of
    Nothing -> M.insert key 1 prevMap
    Just x -> M.insert key (x + 1) prevMap

-- Binary Numbers
binaryStringToDecimal :: String -> Int
binaryStringToDecimal input = sum $ zipWith f input powers
  where
    powers = map (2 ^) $ reverse [0..(length input - 1)]

    f :: Char -> Int -> Int
    f '1' power = power
    f _ _ = 0
