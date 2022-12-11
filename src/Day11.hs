{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1, sepBy1, (<|>), some, MonadParsec (try))
import Text.Megaparsec.Char (eol, char, string)
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, parsePositiveNumber, OccMap, emptyOcc, addKey, incKey)
import qualified Data.Ix as Ix
import Control.Monad (foldM, when)
import Data.List (sort)

dayNum :: Int
dayNum = 11

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO (Maybe Int)
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  result <- processInputEasy input
  findEasySolution result

solveHard :: FilePath -> IO (Maybe Integer)
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  result <- processInputHard input
  findHardSolution result

-------------------- PARSING --------------------
data Operation =
  Addx Int |
  Multx Int |
  Square
  deriving (Show)

data MonkeyRule = MonkeyRule
  { mrOperation :: Operation
  , testDivisible :: Int
  , throwTrue :: Int
  , throwFalse :: Int
  } deriving (Show)

type MonkeyRules = A.Array Int MonkeyRule
type MonkeyItems = HM.HashMap Int (Seq.Seq Int)
type InputType = (MonkeyItems, MonkeyRules)

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = do
  monkeys <- some parseMonkey
  let indices = fst . snd <$> monkeys
  return (HM.fromList (fst <$> monkeys), A.array (minimum indices, maximum indices) (snd <$> monkeys))

parseMonkey :: (MonadLogger m) => ParsecT Void Text m ((Int, Seq.Seq Int), (Int, MonkeyRule))
parseMonkey = do
  string "Monkey "
  i <- parsePositiveNumber
  char ':'
  eol
  startingNums <- parseStartingItems
  op <- parseOperation
  test <- parseTest
  true <- parseThrow
  false <- parseThrow
  eol
  return ((i, Seq.fromList startingNums), (i, MonkeyRule op test true false))

parseStartingItems :: (MonadLogger m) => ParsecT Void Text m [Int]
parseStartingItems = do
  string "  Starting items: "
  nums <- sepBy1 parsePositiveNumber (string ", ")
  eol
  return nums

parseOperation :: (MonadLogger m) => ParsecT Void Text m Operation
parseOperation = do
  string "  Operation: new = old "
  op <- try addOp <|> try multOp <|> squareOp
  eol
  return op
  where
    addOp = string "+ " >> parsePositiveNumber >>= return . Addx
    multOp = string "* " >> parsePositiveNumber >>= return . Multx
    squareOp = string "* old" >> return Square

parseTest :: (MonadLogger m) => ParsecT Void Text m Int
parseTest = do
  string "  Test: divisible by "
  i <- parsePositiveNumber
  eol
  return i

parseThrow :: (MonadLogger m) => ParsecT Void Text m Int
parseThrow = do
  string "    If "
  string "true" <|> string "false"
  string ": throw to monkey "
  i <- parsePositiveNumber
  eol
  return i

-------------------- SOLVING EASY --------------------
type EasySolutionType = OccMap Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy (initialItems, rules) = snd <$> solveStateN rules 20 (initialStateV initialItems)

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution finalOccMap = do
  let results = take 2 . reverse . sort $ M.elems finalOccMap
  return $ Just $ fromIntegral $ product results

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard input@(_, rules) = snd <$> solveStateNHard rules 10000 (initialStateHard input)

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Integer)
findHardSolution finalOccMap = do
  let results = fmap fromIntegral . take 2 . reverse . sort $ M.elems finalOccMap
  return $ Just $ product results

-------------------- SOLUTION PATTERNS --------------------

-- solveFold :: (MonadLogger m) => [LineType] -> m EasySolutionType
-- solveFold = foldM foldLine initialFoldV

-- type FoldType = ()

-- initialFoldV :: FoldType
-- initialFoldV = undefined

-- foldLine :: (MonadLogger m) => FoldType -> LineType -> m FoldType
-- foldLine = undefined

type StateType = (MonkeyItems, OccMap Int)

initialStateV :: MonkeyItems -> StateType
initialStateV i = (i, emptyOcc)

solveStateN :: (MonadLogger m) => MonkeyRules -> Int -> StateType -> m StateType
solveStateN _ 0 st = return st
solveStateN rules n st = do
  st' <- playRound rules st
  solveStateN rules (n - 1) st'

-- Play a full round (all monkeys)
playRound :: (MonadLogger m) => MonkeyRules -> StateType -> m StateType
playRound rules st = foldM (playMonkey rules) st (Ix.range (A.bounds rules))

-- Process all the items a single monkey has
playMonkey :: (MonadLogger m) => MonkeyRules -> StateType -> Int -> m StateType
playMonkey rules st monkey = do
  (newItems, newOcc) <- foldM (playItem rules monkey) st (fst st HM.! monkey)
  return (HM.insert monkey Seq.empty newItems, newOcc)

-- Process a single item.
playItem :: (MonadLogger m) => MonkeyRules -> Int -> StateType -> Int -> m StateType
playItem rules monkey (items, occ1) item = do
  let occ2 = incKey occ1 monkey
      rule = rules A.! monkey
      worry1 = applyOp (mrOperation rule) item
      worry2 = worry1 `quot` 3
      throwTo = if worry2 `mod` testDivisible rule == 0
                  then throwTrue rule else throwFalse rule
      currentThrowToSeq = items HM.! throwTo
      newItems = HM.insert throwTo (currentThrowToSeq Seq.|> worry2) items
  return (newItems, occ2)

applyOp :: Operation -> Int -> Int 
applyOp (Addx x) a = x + a
applyOp (Multx x) a = x * a
applyOp Square a = a * a

-------------------- HARD --------------------

type ModuloHash = HM.HashMap Int Int
type StateType2 = (HM.HashMap Int (Seq.Seq ModuloHash), OccMap Int)

initialStateHard :: (MonkeyItems, MonkeyRules) -> StateType2
initialStateHard (items, rules) = (HM.map (fmap mkModuloHash) items, emptyOcc)
  where
    allDivisibles = testDivisible <$> A.elems rules
    mkModuloHash x = HM.fromList (map (\d -> (d, x `mod` d)) allDivisibles)

solveStateNHard :: (MonadLogger m) => MonkeyRules -> Int -> StateType2 -> m StateType2
solveStateNHard _ 0 st = return st
solveStateNHard rules n st = do
  st' <- playRoundHard rules st
  solveStateNHard rules (n - 1) st'

playRoundHard :: (MonadLogger m) => MonkeyRules -> StateType2 -> m StateType2
playRoundHard rules st = foldM (playMonkeyHard rules) st (Ix.range (A.bounds rules))

playMonkeyHard :: (MonadLogger m) => MonkeyRules -> StateType2 -> Int -> m StateType2
playMonkeyHard rules st monkey = do
  (newItems, newOcc) <- foldM (playItemHard rules monkey) st (fst st HM.! monkey)
  return (HM.insert monkey Seq.empty newItems, newOcc)

playItemHard :: (MonadLogger m) => MonkeyRules -> Int -> StateType2 -> ModuloHash -> m StateType2
playItemHard rules monkey (items, occ1) item = do
  let occ2 = incKey occ1 monkey
      rule = rules A.! monkey
      worry1 = applyOpHard (mrOperation rule) item
      throwTo = if worry1 HM.! testDivisible rule == 0
                  then throwTrue rule else throwFalse rule
      currentThrowToSeq = items HM.! throwTo
      newItems = HM.insert throwTo (currentThrowToSeq Seq.|> worry1) items
  return (newItems, occ2)

applyOpHard :: Operation -> ModuloHash -> ModuloHash
applyOpHard (Addx x) modHash = HM.mapWithKey (\k v1 -> (v1 + x) `mod` k) modHash
applyOpHard (Multx x) modHash = HM.mapWithKey (\k v1 -> (v1 * x) `mod` k) modHash
applyOpHard Square modHash = HM.mapWithKey (\k v1 -> (v1 * v1) `mod` k) modHash

-------------------- BOILERPLATE --------------------
smallFile :: FilePath
smallFile = "inputs_2022/day_" <> show dayNum <> "_small.txt"

largeFile :: FilePath
largeFile = "inputs_2022/day_" <> show dayNum <> "_large.txt"

easySmall :: IO (Maybe Int)
easySmall = solveEasy smallFile

easyLarge :: IO (Maybe Int)
easyLarge = solveEasy largeFile

hardSmall :: IO (Maybe Integer)
hardSmall = solveHard smallFile

hardLarge :: IO (Maybe Integer)
hardLarge = solveHard largeFile