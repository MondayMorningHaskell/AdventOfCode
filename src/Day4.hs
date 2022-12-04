{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT)
import Text.Megaparsec (ParsecT, sepEndBy1)
import Text.Megaparsec.Char (eol, char)
import Data.Void (Void)
import Data.Text (Text)

import Utils (parseFile, parsePositiveNumber)
import Control.Monad (foldM)

dayNum :: Int
dayNum = 4

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
-- type InputType = ()

-- parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
-- parseInput =
--   return ()

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput =
  sepEndBy1 parseLine eol

type InputType = [LineType]
type LineType = ((Int, Int), (Int, Int))

parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
parseLine = do
  a1 <- parsePositiveNumber
  char '-'
  a2 <- parsePositiveNumber
  char ','
  b1 <- parsePositiveNumber
  char '-'
  b2 <- parsePositiveNumber
  return ((a1, a2), (b1, b2))

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy = foldM foldLine initialFoldV

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard = foldM foldPart2 0

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution _ = return Nothing

foldPart2 :: (MonadLogger m) => Int -> LineType -> m Int
foldPart2 prev range = if rangePartiallyContained range
  then return $ prev + 1
  else return prev

rangePartiallyContained :: ((Int, Int), (Int, Int)) -> Bool
rangePartiallyContained ((a1, a2), (b1, b2)) = if a1 <= b1
  then b1 <= a2 
  else a1 <= b2

-------------------- SOLUTION PATTERNS --------------------

solveFold :: (MonadLogger m) => [LineType] -> m EasySolutionType
solveFold = foldM foldLine initialFoldV

type FoldType = Int

initialFoldV :: FoldType
initialFoldV = 0

foldLine :: (MonadLogger m) => FoldType -> LineType -> m FoldType
foldLine prev range = if rangeFullyContained range
  then return $ prev + 1
  else return prev

rangeFullyContained :: ((Int, Int), (Int, Int)) -> Bool
rangeFullyContained ((a1, a2), (b1, b2)) =
  a1 <= b1 && a2 >= b2 ||
  b1 <= a1 && a2 <= b2

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