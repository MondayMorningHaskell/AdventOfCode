module Utils where

readIntsFromFile :: FilePath -> IO [Int]
readIntsFromFile = readLinesFromFile read

readLinesFromFile :: (String -> a) -> FilePath -> IO [a]
readLinesFromFile lineParseFunction filepath =
    map lineParseFunction . lines <$> readFile filepath

countWhere :: (a -> Bool) -> [a] -> Int
countWhere predicate list = length $ filter predicate list