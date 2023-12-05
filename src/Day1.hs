{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Text.Megaparsec (ParsecT, sepEndBy1, many)
import Text.Megaparsec.Char (eol, alphaNumChar)
import Data.Void (Void)
import Data.Text (Text, pack)
import Data.Char (ord, isDigit)
import qualified Data.List as L

import Utils (parseFile)

dayNum :: Int
dayNum = 1

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO (Maybe Int)
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  Just <$> processInputEasy input

solveHard :: FilePath -> IO (Maybe Int)
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  Just <$> processInputHard input

-------------------- PARSING --------------------
type InputType = [String]

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = sepEndBy1 parseLine eol

-- type InputType = [LineType]
type LineType = String

parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
parseLine = many alphaNumChar

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy strings = do
  results <- mapM oneLine strings
  return $ sum results
  where
    f c = ord c - 48 
    oneLine s = case filter isDigit s of
      l@(first : _ : _) -> return $ (f first * 10) + f (last l)
      [i] -> return $ f i * 10 + f i
      _ -> error "Not enough numbers!"

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard strings = sum <$> mapM processString' strings

numbers :: [String]
numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

fNumbers :: [(Int, String)]
fNumbers = zip [1..9] numbers

rNumbers :: [(Int, String)]
rNumbers = zip [1..9] (map reverse numbers)

processString' :: (MonadLogger m) => String -> m Int
processString' input = do
  first <- processString fNumbers input
  final <- processString rNumbers (reverse input)
  let res = (10 * first + final)
  return res

processString :: (MonadLogger m) => [(Int, String)] -> String -> m Int
processString stringOptions "" = logDebugN "Reached end with nothing!" >> error "Reached end with nothing!"
processString stringOptions str@(first : rest) = if isDigit first
    then return $ ord first - 48
    else f stringOptions
    where
      f [] = processString stringOptions rest
      f ((n, v) : restOptions) = if v `L.isPrefixOf` str
        then return n
        else f restOptions

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution _ = return Nothing

-------------------- SOLUTION PATTERNS --------------------

-- solveFold :: (MonadLogger m) => [LineType] -> m EasySolutionType
-- solveFold = foldM foldLine initialFoldV

-- type FoldType = ()

-- initialFoldV :: FoldType
-- initialFoldV = undefined

-- foldLine :: (MonadLogger m) => FoldType -> LineType -> m FoldType
-- foldLine = undefined

-- type StateType = ()

-- initialStateV :: StateType
-- initialStateV = ()

-- solveStateN :: (MonadLogger m) => Int -> StateType -> m StateType
-- solveStateN 0 st = return st
-- solveStateN n st = do
--   st' <- evolveState st
--   solveStateN (n - 1) st'

-- evolveState :: (MonadLogger m) => StateType -> m StateType
-- evolveState st = undefined

-------------------- BOILERPLATE --------------------
smallFile :: FilePath
smallFile = "inputs_2022/day_" <> show dayNum <> "_small.txt"

largeFile :: FilePath
largeFile = "inputs_2022/day_" <> show dayNum <> "_large.txt"

easySmall :: IO (Maybe Int)
easySmall = solveEasy smallFile

easyLarge :: IO (Maybe Int)
easyLarge = solveEasy largeFile

hardSmall :: IO (Maybe Int)
hardSmall = solveHard smallFile

hardLarge :: IO (Maybe Int)
hardLarge = solveHard largeFile
