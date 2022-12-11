{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1, (<|>))
import Text.Megaparsec.Char (eol, string)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, parseSignedInteger)
import qualified Data.HashSet as HS
import Control.Monad (foldM)
import Control.Monad.Cont (lift)
import Data.List.Split (chunksOf)

dayNum :: Int
dayNum = 10

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO (Maybe Int)
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  -- logErrorN (pack . show $ input)
  Just <$> processInputEasy input

solveHard :: FilePath -> IO (Maybe String)
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  result <- processInputHard input
  -- mapM_ (logErrorN . pack) (chunksOf 40 result)
  return $ Just result

-------------------- PARSING --------------------
-- type InputType = ()

-- parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
-- parseInput =
--   return ()

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput =
  sepEndBy1 parseLine eol

data Instruction =
  Noop |
  Addx Int
  deriving (Show)

type InputType = [LineType]
type LineType = Instruction

parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
parseLine = (string "noop" >> return Noop) <|> do
  string "addx "
  Addx <$> parseSignedInteger

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy inputs = accumSignalStrength <$> foldM processInstruction initialMachineState inputs

initialMachineState :: MachineState
initialMachineState = MachineState 1 1 0 ""

data MachineState = MachineState
  { cycleNum :: Int
  , registerValue :: Int
  , accumSignalStrength :: Int
  , renderedString :: String
  }

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

processInstruction :: (MonadLogger m) => MachineState -> Instruction -> m MachineState
processInstruction ms Noop = bumpCycle ms
processInstruction ms0 (Addx i) = do
  ms1 <- bumpCycle ms0
  ms2 <- bumpCycle ms1
  return $ ms2 { registerValue = registerValue ms0 + i}

bumpCycle :: (MonadLogger m) => MachineState -> m MachineState
bumpCycle (MachineState cNum regVal accumSignal render) = do
  let maybeAccum = if HS.member cNum signalCycles
        then regVal * cNum
        else 0
  let newChar = if ((cNum - 1) `mod` 40) `elem` [regVal - 1, regVal, regVal + 1] then '#' else '.'
  return $ MachineState (cNum + 1) regVal (accumSignal + maybeAccum) (newChar : render)

signalCycles :: HS.HashSet Int
signalCycles = HS.fromList [20, 60, 100, 140, 180, 220]

-------------------- SOLVING HARD --------------------
type HardSolutionType = String

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard inputs = reverse . renderedString <$> foldM processInstruction initialMachineState inputs

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

hardSmall :: IO (Maybe String)
hardSmall = solveHard smallFile

hardLarge :: IO (Maybe String)
hardLarge = solveHard largeFile