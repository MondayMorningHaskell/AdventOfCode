{-# LANGUAGE OverloadedStrings #-}

module Day21 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN, logError)
import Text.Megaparsec (ParsecT, sepEndBy1, some, (<|>))
import Text.Megaparsec.Char (eol, letterChar, string, char)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, parsePositiveNumber)
import Data.Int (Int64)
import qualified Data.HashMap.Strict as HM

dayNum :: Int
dayNum = 21

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO (Maybe Int64)
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  Just <$> processInputEasy input

solveHard :: FilePath -> IO (Maybe Int64)
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  Just <$> processInputHard input

-------------------- PARSING --------------------
type CalculationMap = HM.HashMap String Calculation
type InputType = CalculationMap

data Op =
  Plus |
  Minus |
  Times |
  Divided |
  Equals
  deriving (Show, Eq)

data Calculation =
  FinalValue Int64 |
  Operation Op String String |
  HumanVal
  deriving (Show, Eq)

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = HM.fromList <$> sepEndBy1 parseLine eol

type LineType = (String, Calculation)

parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
parseLine = do
  name <- some letterChar
  string ": "
  calc <- parseFinalValue <|> parseOpNode
  return (name, calc)
  where
    parseFinalValue = FinalValue . fromIntegral <$> parsePositiveNumber

parseOp :: (MonadLogger m) => ParsecT Void Text m Op
parseOp =
  (char '+' >> return Plus) <|>
  (char '-' >> return Minus) <|>
  (char '*' >> return Times) <|>
  (char '/' >> return Divided)

parseOpNode :: (MonadLogger m) => ParsecT Void Text m Calculation
parseOpNode = do
  s1 <- some letterChar
  char ' '
  op <- parseOp
  char ' '
  s2 <- some letterChar
  return $ Operation op s1 s2

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int64

processInputEasy :: (MonadFail m, MonadLogger m) => InputType -> m EasySolutionType
processInputEasy calculationMap = solveValue calculationMap "root"

solveValue :: (MonadLogger m, MonadFail m) => CalculationMap -> String -> m Int64
solveValue calculationMap name = case calculationMap HM.! name of
  (FinalValue x) -> return x
  HumanVal -> fail "Can't solve human value! Check with hasHumanVal first."
  (Operation op s1 s2) -> do
    x1 <- solveValue calculationMap s1
    x2 <- solveValue calculationMap s2
    case op of
      Plus -> return $ x1 + x2
      Minus -> return $ x1 - x2
      Times -> return $ x1 * x2
      Divided -> return $ x1 `quot` x2
      Equals -> if x1 == x2 then return 1 else return 0

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadFail m, MonadLogger m) => InputType -> m HardSolutionType
processInputHard input = do
  calculationMap <- updateCalculationsHard input
  -- Note: It doesn't matter what we pass as the initial value here.
  -- We expect "root" to be an "Equals" operation, which discards the value
  -- and replaces it with the solvable number.
  getHumanValForExpectedOutcome calculationMap 0 "root"

updateCalculationsHard :: (MonadLogger m, MonadFail m) => CalculationMap -> m CalculationMap
updateCalculationsHard calculationMap = do
  let map' = HM.insert "humn" HumanVal calculationMap
  case HM.lookup "root" calculationMap of
    Nothing -> fail "Error! Must have root!"
    Just (FinalValue x) -> fail "Error! Root cannot be final!"
    Just HumanVal -> fail "Error! Root cannot be human!"
    Just (Operation _ s1 s2) -> return $ HM.insert "root" (Operation Equals s1 s2) map'

getHumanValForExpectedOutcome :: (MonadLogger m, MonadFail m) => CalculationMap -> Int64 -> String -> m Int64
getHumanValForExpectedOutcome calculationMap expected nodeName = case calculationMap HM.! nodeName of
  HumanVal -> return expected
  (FinalValue _) -> fail "This node doesn't actually depend on human value! Check implementation of hasHumanDep."
  (Operation op s1 s2) -> do
    human1 <- hasHumanDep calculationMap s1
    human2 <- hasHumanDep calculationMap s2
    case (human1, human2) of
      (True, True) -> fail "Both sides have human dependency...can't use this approach!"
      (False, False) -> fail "Neither side has human dependency! Check implementation of hasHumanDep."
      (True, False) -> do
        v2 <- solveValue calculationMap s2
        case op of
          Plus -> getHumanValForExpectedOutcome calculationMap (expected - v2) s1
          Minus -> getHumanValForExpectedOutcome calculationMap (expected + v2) s1
          Times -> getHumanValForExpectedOutcome calculationMap (expected `quot` v2) s1
          Divided -> getHumanValForExpectedOutcome calculationMap (expected * v2) s1
          Equals -> getHumanValForExpectedOutcome calculationMap v2 s1
      (False, True) -> do
        v1 <- solveValue calculationMap s1
        case op of
          Plus -> getHumanValForExpectedOutcome calculationMap (expected - v1) s2
          Minus -> getHumanValForExpectedOutcome calculationMap (v1 - expected) s2
          Times -> getHumanValForExpectedOutcome calculationMap (expected `quot` v1) s2
          Divided -> getHumanValForExpectedOutcome calculationMap (expected * v1) s2
          Equals -> getHumanValForExpectedOutcome calculationMap v1 s2

hasHumanDep :: (MonadLogger m) => CalculationMap -> String -> m Bool
hasHumanDep calculationMap nodeName = case calculationMap HM.! nodeName of
  HumanVal -> return True
  (FinalValue _) -> return False
  (Operation _ s1 s2) -> do
    human1 <- hasHumanDep calculationMap s1
    human2 <- hasHumanDep calculationMap s2
    return $ human1 || human2

-------------------- BOILERPLATE --------------------
smallFile :: FilePath
smallFile = "inputs_2022/day_" <> show dayNum <> "_small.txt"

largeFile :: FilePath
largeFile = "inputs_2022/day_" <> show dayNum <> "_large.txt"

easySmall :: IO (Maybe Int64)
easySmall = solveEasy smallFile

easyLarge :: IO (Maybe Int64)
easyLarge = solveEasy largeFile

hardSmall :: IO (Maybe Int64)
hardSmall = solveHard smallFile

hardLarge :: IO (Maybe Int64)
hardLarge = solveHard largeFile