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
import Text.Megaparsec.Char ( digitChar, char, hspace, eol, hspace1, hexDigitChar )
import Control.Monad.Logger (MonadLogger, logDebugN, logErrorN)
import Data.Functor (($>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Base (VecElem(Int16ElemRep))
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Word (Word64, Word8)
import Control.Monad (mzero)

-- Helpful Types
type Coord2 = (Int, Int)
type Coord2f = (Double, Double)
type Grid2 a = Array Coord2 a

isBounded :: Coord2 -> (Coord2, Coord2) -> Bool
isBounded (x, y) ((mnX, mnY), (mxX, mxY)) =
  mnX <= x && x <= mxX && mnY <= y && y <= mxY

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

hashMapFromNestedLists :: [[a]] -> HashMap Coord2 a
hashMapFromNestedLists inputs = foldl f HM.empty x
  where
    -- x :: [(Int, [(Int, a)])]
    x = zip [0,1..] (map (zip [0,1..]) inputs)

    f :: HashMap Coord2 a -> (Int, [(Int, a)]) -> HashMap Coord2 a
    f prevMap (row, pairs) = foldl (g row) prevMap pairs

    g :: Int -> HashMap Coord2 a -> (Int, a) -> HashMap Coord2 a
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

occLookup :: (Num i, Ord a) => OccMapI a i -> a -> i
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
getNeighbors8 grid = getNeighbors8Flex (0, 0) (maxRow, maxCol)
  where
    (maxRow, maxCol) = maximum $ HM.keys grid

getNeighbors8Unbounded :: Coord2 -> [Coord2]
getNeighbors8Unbounded = getNeighbors8Flex (minBound, minBound) (maxBound, maxBound)

getNeighbors8Flex :: Coord2 -> Coord2 -> Coord2 -> [Coord2]
getNeighbors8Flex (minRow, minCol) (maxRow, maxCol) (row, col) = catMaybes
  [maybeUpLeft, maybeUp, maybeUpRight, maybeLeft, maybeRight, maybeDownLeft, maybeDown, maybeDownRight]
  where
    maybeUp = if row > minRow then Just (row - 1, col) else Nothing
    maybeUpRight = if row > minRow && col < maxCol then Just (row - 1, col + 1) else Nothing
    maybeRight = if col < maxCol then Just (row, col + 1) else Nothing
    maybeDownRight = if row < maxRow && col < maxCol then Just (row + 1, col + 1) else Nothing
    maybeDown = if row < maxRow then Just (row + 1, col) else Nothing
    maybeDownLeft = if row < maxRow && col > minCol then Just (row + 1, col - 1) else Nothing
    maybeLeft = if col > minCol then Just (row, col - 1) else Nothing
    maybeUpLeft = if row > minRow && col > minCol then Just (row - 1, col - 1) else Nothing

-- Binary
data Bit = Zero | One
  deriving (Eq, Ord)

instance Show Bit where
  show Zero = "0"
  show One = "1"

parseHexadecimal :: (MonadLogger m) => ParsecT Void Text m String
parseHexadecimal = some hexDigitChar

parseHexChar :: (MonadLogger m) => Char -> MaybeT m [Bit]
parseHexChar '0' = return [Zero, Zero, Zero, Zero]
parseHexChar '1' = return [Zero, Zero, Zero, One]
parseHexChar '2' = return [Zero, Zero, One, Zero]
parseHexChar '3' = return [Zero, Zero, One, One]
parseHexChar '4' = return [Zero, One, Zero, Zero]
parseHexChar '5' = return [Zero, One, Zero, One]
parseHexChar '6' = return [Zero, One, One, Zero]
parseHexChar '7' = return [Zero, One, One, One]
parseHexChar '8' = return [One, Zero, Zero, Zero]
parseHexChar '9' = return [One, Zero, Zero, One]
parseHexChar 'A' = return [One, Zero, One, Zero]
parseHexChar 'B' = return [One, Zero, One, One]
parseHexChar 'C' = return [One, One, Zero, Zero]
parseHexChar 'D' = return [One, One, Zero, One]
parseHexChar 'E' = return [One, One, One, Zero]
parseHexChar 'F' = return [One, One, One, One]
parseHexChar c = logErrorN ("Invalid Hex Char: " <> pack [c]) >> mzero

bitsToDecimal8 :: [Bit] -> Word8
bitsToDecimal8 bits = if length bits > 8
  then error ("Too long! Use bitsToDecimal64! " ++ show bits)
  else btd8 0 1 (reverse bits)
    where
      btd8 :: Word8 -> Word8 -> [Bit] -> Word8
      btd8 accum _ [] = accum
      btd8 accum mult (b : rest) = case b of
        Zero -> btd8 accum (mult * 2) rest
        One -> btd8 (accum + mult) (mult * 2) rest

bitsToDecimal64 :: [Bit] -> Word64
bitsToDecimal64 bits = if length bits > 64
  then error ("Too long! Use bitsToDecimalInteger! " ++ (show $ bits))
  else btd64 0 1 (reverse bits)
    where
      btd64 :: Word64 -> Word64 -> [Bit] -> Word64
      btd64 accum _ [] = accum
      btd64 accum mult (b : rest) = case b of
        Zero -> btd64 accum (mult * 2) rest
        One -> btd64 (accum + mult) (mult * 2) rest

bitsToDecimalInteger :: (MonadLogger m) => [Bit] -> MaybeT m Integer
bitsToDecimalInteger bits = btd 0 1 (reverse bits)
    where
      btd :: (Monad m) => Integer-> Integer -> [Bit] -> m Integer
      btd accum _ [] = return accum
      btd accum mult (b : rest) = case b of
        Zero -> btd accum (mult * 2) rest
        One -> btd (accum + mult) (mult * 2) rest
