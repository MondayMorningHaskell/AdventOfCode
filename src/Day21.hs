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
type DependencyMap = HM.HashMap String [String]
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

data Value =
  SolvedValue Int64 | UnsolvedValue String
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
    parseOp =
      (char '+' >> return Plus) <|>
      (char '-' >> return Minus) <|>
      (char '*' >> return Times) <|>
      (char '/' >> return Divided)
    parseOpNode = do
      s1 <- some letterChar
      char ' '
      op <- parseOp
      char ' '
      s2 <- some letterChar
      return $ Operation op s1 s2

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int64

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy calculationMap = solveValue calculationMap "root"

  -- Filter Calculation Map By FinalValues
  -- Plug These into Calculation Map by using dependencyMap
  -- Filter Calculation Map by Ops with Solved Values
  -- Replace with Final Values
  -- Recurse

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

solveValue :: (MonadLogger m) => CalculationMap -> String -> m Int64
solveValue calculationMap name = case HM.lookup name calculationMap of
  Nothing -> logErrorN ("Invalid Name: " <> pack name <> "!") >> return minBound
  Just HumanVal -> logErrorN "Can't solve human value! Check with hasHumanVal first." >> return minBound
  Just (FinalValue x) -> return x
  Just (Operation op s1 s2) -> do
    x1 <- solveValue calculationMap s1
    x2 <- solveValue calculationMap s2
    case op of
      Plus -> return $ x1 + x2
      Minus -> return $ x1 - x2
      Times -> return $ x1 * x2
      Divided -> return $ x1 `quot` x2
      Equals -> logErrorN "Invalid use of equals...can only apply to 'root'" >> return minBound

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

updateCalculationsHard :: (MonadLogger m) => CalculationMap -> m CalculationMap
updateCalculationsHard calculationMap = do
  let map' = HM.insert "humn" HumanVal calculationMap
  case HM.lookup "root" calculationMap of
    Nothing -> logErrorN "Error! Must have root!" >> return calculationMap
    Just (FinalValue x) -> logErrorN "Error! Root cannot be final!" >> return calculationMap
    Just HumanVal -> logErrorN "Error! Root cannot be human!" >> return calculationMap
    Just (Operation _ s1 s2) -> return $ HM.insert "root" (Operation Equals s1 s2) map'

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard input = do
  input' <- updateCalculationsHard input
  case HM.lookup "root" input' of
    Nothing -> return minBound
    Just (FinalValue x) -> return (-4)
    Just HumanVal -> return (-5)
    Just (Operation _ s1 s2) -> do
      human1 <- hasHumanDep input' s1
      human2 <- hasHumanDep input' s2
      case (human1, human2) of
        (True, True) -> logErrorN "Both sides have human deps...I need a new approach." >> return (-1)
        (False, False) -> logErrorN "Neither side has human dep...probably didn't substitute right." >> return (-2)
        (True, False) -> do
          v2 <- solveValue input' s2
          getHumanValForExpectedOutcome input' v2 s1
        (False, True) -> do
          v1 <- solveValue input' s1
          getHumanValForExpectedOutcome input' v1 s2

getHumanValForExpectedOutcome :: (MonadLogger m) => CalculationMap -> Int64 -> String -> m Int64
getHumanValForExpectedOutcome calculationMap expected nodeName = do
  -- logErrorN (pack nodeName)
  case calculationMap HM.! nodeName of
    HumanVal -> return expected
    (FinalValue _) -> logErrorN "This node doesn't actually depend on human value!" >> return (-3)
    (Operation op s1 s2) -> do
      human1 <- hasHumanDep calculationMap s1
      human2 <- hasHumanDep calculationMap s2
      case (human1, human2) of
        (True, True) -> logErrorN "Both sides have human deps...I need a new approach." >> return (-1)
        (False, False) -> logErrorN "Neither side has human dep...probably didn't substitute right." >> return (-2)
        (True, False) -> do
          v2 <- solveValue calculationMap s2
          case op of
            Plus -> getHumanValForExpectedOutcome calculationMap (expected - v2) s1
            Minus -> getHumanValForExpectedOutcome calculationMap (expected + v2) s1
            Times -> getHumanValForExpectedOutcome calculationMap (expected `quot` v2) s1
            Divided -> getHumanValForExpectedOutcome calculationMap (expected * v2) s1
            Equals -> logErrorN "Equals is reserved for root." >> return (-6)
        (False, True) -> do
          v1 <- solveValue calculationMap s1
          case op of
            Plus -> getHumanValForExpectedOutcome calculationMap (expected - v1) s2
            Minus -> getHumanValForExpectedOutcome calculationMap (v1 - expected) s2
            Times -> getHumanValForExpectedOutcome calculationMap (expected `quot` v1) s2
            Divided -> getHumanValForExpectedOutcome calculationMap (expected * v1) s2
            Equals -> logErrorN "Equals is reserved for root." >> return (-7)


hasHumanDep :: (MonadLogger m) => CalculationMap -> String -> m Bool
hasHumanDep calculationMap nodeName = do
  let node = calculationMap HM.! nodeName
  case node of
    HumanVal -> return True
    (FinalValue _) -> return False
    (Operation _ s1 s2) -> do
      human1 <- hasHumanDep calculationMap s1
      human2 <- hasHumanDep calculationMap s2
      return $ human1 || human2

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

easySmall :: IO (Maybe Int64)
easySmall = solveEasy smallFile

easyLarge :: IO (Maybe Int64)
easyLarge = solveEasy largeFile

hardSmall :: IO (Maybe Int64)
hardSmall = solveHard smallFile

hardLarge :: IO (Maybe Int64)
hardLarge = solveHard largeFile