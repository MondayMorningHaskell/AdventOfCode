{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1, some)
import Text.Megaparsec.Char (eol, letterChar)
import Data.Void (Void)
import Data.Text (Text, pack)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq

import Utils (parseFile, OccMap, emptyOcc, incKey, decKey)
import Control.Monad (foldM)

dayNum :: Int
dayNum = 6

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
type InputType = String

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = some letterChar

-- parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
-- parseInput =
--   sepEndBy1 parseLine eol

-- type InputType = [LineType]
-- type LineType = ()

-- parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
-- parseLine = return ()

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy = processChars 4

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard = processChars 14

processChars :: (MonadLogger m) => Int -> [Char] -> m Int
processChars numCharsNeeded input = if length input < numCharsNeeded
  then logErrorN "Not enough chars!" >> return maxBound
  else do
    let (firstChars, rest) = splitAt (numCharsNeeded - 1) input
        seq = Seq.fromList firstChars
        occ = foldl incKey emptyOcc firstChars
    processTail numCharsNeeded (numCharsNeeded, seq, occ) rest

processTail :: (MonadLogger m) => Int -> (Int, Seq.Seq Char, OccMap Char) -> [Char] -> m Int
processTail _ _ [] = logErrorN "No remaining chars!" >> return maxBound
processTail numCharsNeeded (count, seq, occ) (c : cs) = case Seq.viewl seq of
  Seq.EmptyL -> logErrorN "Sequence is empty!" >> return maxBound
  (first Seq.:< rest) -> do
    let occ' = incKey occ c
    if M.size occ' == numCharsNeeded
      then return count
      else processTail numCharsNeeded (count + 1, rest Seq.|> c, decKey occ' first) cs

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution _ = return Nothing

-------------------- SOLUTION PATTERNS --------------------

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