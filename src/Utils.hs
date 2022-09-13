{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Data.Map (Map)
import qualified Data.Map as M
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
parse2DMapSpace rowsPerMap = createMap <$> (eol >> parseNumbers rowsPerMap)
  where

    createMap :: [[Int]] -> HashMap (Int, Int) Int
    createMap inputs =
      let x = zip [0,1..] (map (zip [0,1..]) inputs)
      in  foldl f HM.empty x
    
    f :: HashMap (Int, Int) Int -> (Int, [(Int, Int)]) -> HashMap (Int, Int) Int
    f prevMap (row, pairs) = foldl (g row) prevMap pairs

    g :: Int -> HashMap (Int, Int) Int -> (Int, Int) -> HashMap (Int, Int) Int
    g row prevMap (col, val) = HM.insert (row, col) val prevMap

parseNumbers :: forall m. (Monad m) => Int -> ParsecT Void Text m [[Int]]
parseNumbers = parseNumbersTail []
  where
    parseNumbersTail :: [[Int]] -> Int -> ParsecT Void Text m [[Int]]
    parseNumbersTail prev 0 = return $ reverse prev
    parseNumbersTail prev rowsRemaining = do
      newLine <- parseSpacedInts <* eol
      parseNumbersTail (newLine : prev) (rowsRemaining - 1)

-- Solution Patterns
countWhere :: (a -> Bool) -> [a] -> Int
countWhere predicate list = length $ filter predicate list

-- Common Structures
type OccMap a = Map a Word

emptyOcc = M.empty

incKey :: (Ord a) => OccMap a -> a -> OccMap a
incKey prevMap key = case M.lookup key prevMap of
    Nothing -> M.insert key 1 prevMap
    Just x -> M.insert key (x + 1) prevMap

incKeyWithOcc :: (Ord a) => OccMap a -> a -> (OccMap a, Word)
incKeyWithOcc prevMap key = case M.lookup key prevMap of
    Nothing -> (M.insert key 1 prevMap, 1)
    Just x -> (M.insert key (x + 1) prevMap, x + 1)

-- Binary Numbers
binaryStringToDecimal :: String -> Int
binaryStringToDecimal input = sum $ zipWith f input powers
  where
    powers = map (2 ^) $ reverse [0..(length input - 1)]

    f :: Char -> Int -> Int
    f '1' power = power
    f _ _ = 0
