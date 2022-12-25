{-# LANGUAGE OverloadedStrings #-}

module Day25 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1, some, (<|>))
import Text.Megaparsec.Char (eol, char)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile)
import Data.List (find)

dayNum :: Int
dayNum = 25

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO String
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  result <- processInputEasy input
  findEasySolution result

solveHard :: FilePath -> IO (Maybe Int)
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  result <- processInputHard input
  findHardSolution result

-------------------- PARSING --------------------
parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = sepEndBy1 parseLine eol

type InputType = [LineType]
type LineType = [Int]

parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
parseLine = some parseSnafuNums

parseSnafuNums :: (MonadLogger m) => ParsecT Void Text m Int
parseSnafuNums =
  (char '2' >> return 2) <|>
  (char '1' >> return 1) <|>
  (char '0' >> return 0) <|>
  (char '-' >> return (-1)) <|>
  (char '=' >> return (-2))

-------------------- SOLVING EASY --------------------
type EasySolutionType = Integer

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy inputs = do
  let decimalNums = map translateSnafuNum inputs
  return (sum decimalNums)

translateSnafuNum :: [Int] -> Integer
translateSnafuNum nums = foldl addSnafu 0 (zip [0,1..] (reverse nums))
  where
    addSnafu prev (index, num) = fromIntegral (5 ^ index * num) + prev

findEasySolution :: (MonadLogger m) => EasySolutionType -> m String
findEasySolution number = do
  finalSnafuInts <- decimalToSnafuTail first5Power number []
  return (intToSnafuChar <$> finalSnafuInts)
  where
    first5Power = head [n | n <- map (5^) [0,1..], n >= number]

decimalToSnafuTail :: (MonadLogger m) => Integer -> Integer -> [Int] -> m [Int]
decimalToSnafuTail power5 remainder accum
  | abs remainder < 3 = return $ reverse (fromIntegral remainder : accum)
  | remainder > (power5 `quot` 2) = do
    let add1 = if null accum then [1] else head accum + 1 : tail accum
    recursionResult <- decimalToSnafuTail power5 (power5 - remainder) []
    return $ reverse add1 ++ map ((-1) *) recursionResult
  | remainder >= 2 * next5 = decimalToSnafuTail next5 (remainder - (2 * next5)) (2 : accum)
  | remainder >= power5 `quot` 5 = decimalToSnafuTail next5 (remainder - next5) (1 : accum)
  | otherwise = decimalToSnafuTail next5 remainder (0 : accum)
  where
    next5 = power5 `quot` 5

intToSnafuChar :: Int -> Char
intToSnafuChar 2 = '2'
intToSnafuChar 1 = '1'
intToSnafuChar (-1) = '-'
intToSnafuChar (-2) = '='
intToSnafuChar _ = '0'

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard _ = undefined

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution _ = return Nothing

-------------------- BOILERPLATE --------------------
smallFile :: FilePath
smallFile = "inputs_2022/day_" <> show dayNum <> "_small.txt"

largeFile :: FilePath
largeFile = "inputs_2022/day_" <> show dayNum <> "_large.txt"

easySmall :: IO String
easySmall = solveEasy smallFile

easyLarge :: IO String
easyLarge = solveEasy largeFile

hardSmall :: IO (Maybe Int)
hardSmall = solveHard smallFile

hardLarge :: IO (Maybe Int)
hardLarge = solveHard largeFile