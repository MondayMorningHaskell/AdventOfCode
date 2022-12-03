{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Text.Megaparsec (ParsecT, sepEndBy1, (<|>))
import Text.Megaparsec.Char (eol, char)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile)
import Control.Monad (foldM, when)

dayNum :: Int
dayNum = 2

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

data RPS = Rock | Paper | Scissors
  deriving (Show, Eq)

data ExpectedResult = Loss | Draw | Win
  deriving (Show, Eq)

rpsToResult :: RPS -> ExpectedResult
rpsToResult Rock = Loss
rpsToResult Paper = Draw
rpsToResult Scissors = Win

translate :: (RPS, RPS) -> (RPS, ExpectedResult)
translate (first, second) = (first, rpsToResult second)

type InputType = [LineType]
type LineType = (RPS, RPS)

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = sepEndBy1 parseLine eol

parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
parseLine = do
  first <- parseRPS
  char ' '
  second <- parseRPS
  return (first, second)

parseRPS :: ParsecT Void Text m RPS
parseRPS = parseRock <|> parsePaper <|> parseScissors
  where
    parseRock = (char 'A' <|> char 'X') >> return Rock
    parsePaper = (char 'B' <|> char 'Y') >> return Paper
    parseScissors = (char 'C' <|> char 'Z') >> return Scissors

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy = solveFold

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard inputs = foldM foldExpResult initialFoldV (map translate inputs)

foldExpResult :: (MonadLogger m) => Int -> (RPS, ExpectedResult) -> m Int
foldExpResult prevScore (oppPlay, result) = return $ prevScore + evalMatchHard (oppPlay, result)

evalMatchHard :: (RPS, ExpectedResult) -> Int
evalMatchHard (Rock, Win) = 8      -- Play Paper (2 + 6)
evalMatchHard (Rock, Draw) = 4     -- Play Rock (1 + 3)
evalMatchHard (Rock, Loss) = 3     -- Play Scissors (3 + 0)
evalMatchHard (Paper, Win) = 9     -- Play Scissors (3 + 6)
evalMatchHard (Paper, Draw) = 5    -- Play Paper (2 + 3)
evalMatchHard (Paper, Loss) = 1    -- Play Rock (1 + 0)
evalMatchHard (Scissors, Win) = 7  -- Play Rock (1 + 6)
evalMatchHard (Scissors, Draw) = 6 -- Play Scissors (3 + 3)
evalMatchHard (Scissors, Loss) = 2 -- Play Paper (2 + 0)

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution _ = return Nothing

-------------------- SOLUTION PATTERNS --------------------

solveFold :: (MonadLogger m) => [LineType] -> m EasySolutionType
solveFold = foldM foldLine initialFoldV

type FoldType = Int

initialFoldV :: FoldType
initialFoldV = 0

foldLine :: (MonadLogger m) => FoldType -> LineType -> m FoldType
foldLine prevScore (first, second) = return $ prevScore + (evalMatch (first, second) + scoreRps second)

scoreRps :: RPS -> Int
scoreRps Rock = 1
scoreRps Paper = 2
scoreRps Scissors = 3

evalMatch :: (RPS, RPS) -> Int
evalMatch (Rock, Rock) = 3
evalMatch (Rock, Paper) = 6
evalMatch (Rock, Scissors) = 0
evalMatch (Paper, Rock) = 0
evalMatch (Paper, Paper) = 3
evalMatch (Paper, Scissors) = 6
evalMatch (Scissors, Rock) = 6
evalMatch (Scissors, Paper) = 0
evalMatch (Scissors, Scissors) = 3

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