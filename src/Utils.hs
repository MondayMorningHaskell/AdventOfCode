{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Data.Array (Array)
import qualified Data.Array as A
import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec ( some, Parsec, sepBy, runParser, ParsecT, runParserT, someTill, MonadParsec (eof, try), sepEndBy1, sepBy1, (<|>), many )
import Text.Megaparsec.Char ( digitChar, char, hspace, eol, hspace1 )
import Control.Monad.Logger (MonadLogger, logDebugN)
import Data.Functor (($>))
import Control.Monad.IO.Class (MonadIO, liftIO)

-- Helpful Types
type Coord2 = (Int, Int)
type Coord2f = (Double, Double)
type Grid2 a = Array Coord2 a

-- Reading From Files
readIntsFromFile :: FilePath -> IO [Int]
readIntsFromFile = readLinesFromFile read

readStringsFromFile :: FilePath -> IO [String]
readStringsFromFile = readLinesFromFile id

readLinesFromFile :: (String -> a) -> FilePath -> IO [a]
readLinesFromFile lineParseFunction filepath =
    map lineParseFunction . lines <$> readFile filepath

parseLinesFromFile :: (MonadIO m) => ParsecT Void Text m a -> FilePath -> m [a]
parseLinesFromFile parser filepath = do
  input <- pack <$> liftIO (readFile filepath)
  result <- runParserT (sepEndBy1 parser eol) "Utils.hs" input
  case result of
    Left e -> error $ "Failed to parse: " ++ show e
    Right x -> return x

parseFile :: (MonadIO m) => ParsecT Void Text m a -> FilePath -> m a
parseFile parser filepath = do
  input <- pack <$> liftIO (readFile filepath)
  result <- runParserT parser "Utils.hs" input
  case result of
    Left e -> error $ "Failed to parse: " ++ show e
    Right x -> return x

{- Parsers -}
rpd :: Show a => Parsec Void Text a -> String -> IO ()
rpd = runParserDebug

rpdl :: (Show a, MonadLogger m) => ParsecT Void Text m a -> [String] -> m a
rpdl = runParserDebugLines

runParserDebug :: (Show a) => Parsec Void Text a -> String -> IO ()
runParserDebug parser input = case runParser parser "Utils.hs" (pack input) of
  Left e -> putStr "Error: " >> print e
  Right x -> print x

runParserDebugLines :: (Show a, MonadLogger m) => ParsecT Void Text m a -> [String] -> m a
runParserDebugLines parser inputs = do
  result <- runParserT parser "Utils.hs" (pack $ unlines inputs)
  case result of
    Left e -> logDebugN "Error: " >> error (show e)
    Right x -> logDebugN (pack . show $ x) >> return x

parsePositiveNumber :: (Monad m) => ParsecT Void Text m Int
parsePositiveNumber = read <$> some digitChar

-- Positive numbers only
parseCSVInts :: (Monad m) => ParsecT Void Text m [Int]
parseCSVInts = sepBy parsePositiveNumber (char ',')

parseSpacedInts :: (Monad m) => ParsecT Void Text m [Int]
parseSpacedInts = hspace >> sepBy1 parsePositiveNumber hspace1

parse2DMapSpaceMany :: (Monad m) => Int -> ParsecT Void Text m [HashMap (Int, Int) Int]
parse2DMapSpaceMany rowsPerMap = some (parse2DMapSpace rowsPerMap)

parse2DMapSpace :: (Monad m) => Int -> ParsecT Void Text m (HashMap (Int, Int) Int)
parse2DMapSpace rowsPerMap = hashMapFromNestedLists <$> (eol >> parseNumbers rowsPerMap)

parseNumbers :: forall m. (Monad m) => Int -> ParsecT Void Text m [[Int]]
parseNumbers = parseNumbersTail []
  where
    parseNumbersTail :: [[Int]] -> Int -> ParsecT Void Text m [[Int]]
    parseNumbersTail prev 0 = return $ reverse prev
    parseNumbersTail prev rowsRemaining = do
      newLine <- parseSpacedInts <* eol
      parseNumbersTail (newLine : prev) (rowsRemaining - 1)

-- Only single digit numbers
parse2DDigitArray :: (Monad m) => ParsecT Void Text m (Grid2 Int)
parse2DDigitArray = digitsToArray <$> sepEndBy1 parseDigitLine eol

digitsToArray :: [[Int]] -> Grid2 Int
digitsToArray inputs = A.listArray ((0, 0), (length inputs - 1, length (head inputs) - 1)) (concat inputs)

parseDigitLine :: ParsecT Void Text m [Int]
parseDigitLine = fmap digitToInt <$> some digitChar

hashMapFromNestedLists :: [[Int]] -> HashMap Coord2 Int
hashMapFromNestedLists inputs = foldl f HM.empty x
  where
    x :: [(Int, [(Int, Int)])]
    x = zip [0,1..] (map (zip [0,1..]) inputs)

    f :: HashMap Coord2 Int -> (Int, [(Int, Int)]) -> HashMap Coord2 Int
    f prevMap (row, pairs) = foldl (g row) prevMap pairs

    g :: Int -> HashMap Coord2 Int -> Coord2 -> HashMap Coord2 Int
    g row prevMap (col, val) = HM.insert (row, col) val prevMap


parse2DDigits :: (Monad m) => ParsecT Void Text m [[Int]]
parse2DDigits = sepEndBy1 parseDigitLine eol

parse2DDigitHashMap :: (Monad m) => ParsecT Void Text m (HashMap Coord2 Int)
parse2DDigitHashMap = hashMapFromNestedLists <$> parse2DDigits

-- Solution Patterns
countWhere :: (a -> Bool) -> [a] -> Int
countWhere predicate list = length $ filter predicate list

-- Common Structures
type OccMap a = OccMapI a Word
type OccMapBig a = OccMapI a Integer
type OccMapI a i = Map a i

emptyOcc :: OccMap a
emptyOcc = M.empty

incKey :: (Ord a, Integral i) => OccMapI a i -> a -> OccMapI a i
incKey prevMap key = addKey prevMap key 1

addKey :: (Ord a, Integral i) => OccMapI a i -> a -> i -> OccMapI a i
addKey prevMap key count = case M.lookup key prevMap of
    Nothing -> M.insert key count prevMap
    Just x -> M.insert key (x + count) prevMap

incKeyWithOcc :: (Ord a, Integral i) => OccMapI a i -> a -> (OccMapI a i, i)
incKeyWithOcc prevMap key = case M.lookup key prevMap of
    Nothing -> (M.insert key 1 prevMap, 1)
    Just x -> (M.insert key (x + 1) prevMap, x + 1)

occLookup :: (Ord a) => OccMap a -> a -> Word
occLookup prevMap key = fromMaybe 0 (M.lookup key prevMap)

-- Binary Numbers
binaryStringToDecimal :: String -> Int
binaryStringToDecimal input = sum $ zipWith f input powers
  where
    powers = map (2 ^) $ reverse [0..(length input - 1)]

    f :: Char -> Int -> Int
    f '1' power = power
    f _ _ = 0

-- 2D Grid Helpers
-- TODO: Should also do for Hash Map
getNeighbors :: Grid2 a -> Coord2 -> [Coord2]
getNeighbors grid (row, col) = catMaybes [maybeUp, maybeDown, maybeLeft, maybeRight]
  where
    (maxRow, maxCol) = snd . A.bounds $ grid
    maybeUp = if row > 0 then Just (row - 1, col) else Nothing
    maybeDown = if row < maxRow then Just (row + 1, col) else Nothing
    maybeLeft = if col > 0 then Just (row, col - 1) else Nothing
    maybeRight = if col < maxCol then Just (row, col + 1) else Nothing

getNeighborsAssoc :: Grid2 a -> Coord2 -> [(Coord2, a)]
getNeighborsAssoc grid coord = getAssocs grid (getNeighbors grid coord)

getAssocs :: Grid2 a -> [Coord2] -> [(Coord2, a)]
getAssocs grid = map (\c -> (c, grid A.! c))

-- TODO: Should also do for Array
getNeighbors8 :: HashMap Coord2 a -> Coord2 -> [Coord2]
getNeighbors8 grid (row, col) = catMaybes
  [maybeUp, maybeUpRight, maybeRight, maybeDownRight, maybeDown, maybeDownLeft, maybeLeft, maybeUpLeft]
  where
    (maxRow, maxCol) = maximum $ HM.keys grid
    maybeUp = if row > 0 then Just (row - 1, col) else Nothing
    maybeUpRight = if row > 0 && col < maxCol then Just (row - 1, col + 1) else Nothing
    maybeRight = if col < maxCol then Just (row, col + 1) else Nothing
    maybeDownRight = if row < maxRow && col < maxCol then Just (row + 1, col + 1) else Nothing
    maybeDown = if row < maxRow then Just (row + 1, col) else Nothing
    maybeDownLeft = if row < maxRow && col > 0 then Just (row + 1, col - 1) else Nothing
    maybeLeft = if col > 0 then Just (row, col - 1) else Nothing
    maybeUpLeft = if row > 0 && col > 0 then Just (row - 1, col - 1) else Nothing
