{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1, some)
import Text.Megaparsec.Char (eol, letterChar)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile)
import Control.Monad (foldM)
import Data.Char (isUpper, ord)
import Data.List (nub)
import Data.List.Split (chunksOf)

dayNum :: Int
dayNum = 3

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
parseInput = sepEndBy1 parseLine eol

type InputType = [LineType]
type LineType = String

parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
parseLine = some letterChar

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy = solveFold

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = Int

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard allLines = foldM foldHard 0 (chunksOf 3 allLines)

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution _ = return Nothing

foldHard :: (MonadLogger m) => Int -> [String] -> m Int
foldHard prevScore [s1, s2, s3] = do
  case all3 of
    [c] -> return (prevScore + scoreChar c)
    cs -> logErrorN ("Invalid chars in all 3 ! " <> (pack . show $ cs)) >> return prevScore
  where
    s1AndS2 = filter (`elem` s2) s1
    all3 = nub $ filter (`elem` s3) s1AndS2
foldHard prevScore inputs = logErrorN ("Invalid inputs (should be size 3) " <> (pack . show $ inputs)) >> return prevScore

-------------------- SOLUTION PATTERNS --------------------

solveFold :: (MonadLogger m) => [LineType] -> m EasySolutionType
solveFold = foldM foldLine initialFoldV

type FoldType = Int

initialFoldV :: FoldType
initialFoldV = 0

foldLine :: (MonadLogger m) => FoldType -> LineType -> m FoldType
foldLine prevScore inputLine = do
  case charsInBoth of
    [c] -> return (prevScore + scoreChar c)
    cs -> logErrorN ("Invalid chars in both sides! " <> (pack . show $ cs)) >> return prevScore
  where
    compartmentSize = length inputLine `quot` 2
    (firstHalf, secondHalf) = splitAt compartmentSize inputLine
    charsInBoth = nub $ filter (`elem` secondHalf) firstHalf

scoreChar :: Char -> Int
scoreChar c = if isUpper c
  then ord c - 38
  else ord c - 96

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