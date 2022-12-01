{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Text.Megaparsec (ParsecT, sepEndBy1, some)
import Text.Megaparsec.Char (eol)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, parsePositiveNumber)
import Data.List (sort)

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
type InputType = [[Int]]

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput =
  sepEndBy1 parseIntLines eol
  where
    parseIntLines = some parseIntLine
    parseIntLine = do
      i <- parsePositiveNumber
      eol
      return i

-- parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
-- parseInput =
--   sepEndyBy1 parseLine eol

-- type InputType = [LineType]
-- type LineType = ()

-- parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
-- parseLine = return ()

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy intLists = return $ maximum (map sum intLists)

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution i = return (Just i)

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard intLists = return $ sum $ take 3 $ reverse $ sort (map sum intLists)

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution i = return (Just i)

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