{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Control.Monad (void, foldM)
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Text.Megaparsec (ParsecT, sepEndBy1, (<|>), sepBy1, some)
import Text.Megaparsec.Char (eol, letterChar, char, digitChar, string)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, parsePositiveNumber)
import Control.Monad.Cont (lift)
import Data.Maybe (fromMaybe)
import Data.List (sort)

dayNum :: Int
dayNum = 5

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO (Maybe String)
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  result <- processInputEasy input
  findEasySolution result

solveHard :: FilePath -> IO (Maybe String)
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  result <- processInputHard input
  findEasySolution result

-------------------- PARSING --------------------
type CrateStacks = HashMap Int [Char]
data Move = Move
  { numCrates :: Int
  , sourceStack :: Int
  , destStack :: Int
  } deriving (Show)

type InputType = (CrateStacks, [Move])

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = do
  crateStack <- parseCrateStack
  eol
  moves <- sepEndBy1 parseMove eol
  return (crateStack, moves)

parseCrateStack :: (MonadLogger m) => ParsecT Void Text m CrateStacks
parseCrateStack = do
  crateLines <- sepEndBy1 parseCrateLine eol
  parseCrateNumbers
  lift $ buildCrateStack (reverse crateLines)

buildCrateStack :: (MonadLogger m) => [[Maybe Char]] -> m CrateStacks
buildCrateStack crateLines = return $ foldl addCrateLine HM.empty crateLines
  where
    addCrateLine :: CrateStacks -> [Maybe Char] -> CrateStacks
    addCrateLine prevStacks lineChars = foldl addCrate prevStacks (zip [1,2..] lineChars)

addCrate :: CrateStacks -> (Int, Maybe Char) -> CrateStacks
addCrate prev (_, Nothing) = prev
addCrate prev (i, Just c) =
  let prevStackI = fromMaybe [] (HM.lookup i prev)
  in  HM.insert i (c : prevStackI) prev

parseCrateNumbers :: (MonadLogger m) => ParsecT Void Text m ()
parseCrateNumbers = void $ some (digitChar <|> char ' ') >> eol

parseCrateLine :: (MonadLogger m) => ParsecT Void Text m [Maybe Char]
parseCrateLine = sepEndBy1 parseCrateChar (char ' ')

parseCrateChar :: (MonadLogger m) => ParsecT Void Text m (Maybe Char)
parseCrateChar = crate <|> noCrate
  where
    crate = do
      char '['
      c <- letterChar
      char ']'
      return $ Just c
    noCrate = string "   " >> return Nothing

parseMove :: (MonadLogger m) => ParsecT Void Text m Move
parseMove = do
  string "move "
  numCrates <- parsePositiveNumber
  string " from "
  sourceIndex <- parsePositiveNumber
  string " to "
  destIndex <- parsePositiveNumber
  return $ Move numCrates sourceIndex destIndex

-------------------- SOLVING EASY --------------------
type EasySolutionType = CrateStacks

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy (stacks, moves) = solveFold stacks moves

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe String)
findEasySolution crateStacks = do
  let sortedResults = sort (HM.toList crateStacks)
  return $ Just $ map safeHead (snd <$> sortedResults)

safeHead :: [Char] -> Char
safeHead [] = ' '
safeHead (c : _) = c

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard (stacks, moves) = foldM foldLineHard stacks moves

foldLineHard :: (MonadLogger m) => FoldType -> Move -> m FoldType
foldLineHard crateStacks (Move num s d) = do
  let sourceStack = fromMaybe [] (HM.lookup s crateStacks)
      destStack = fromMaybe [] (HM.lookup d crateStacks)
  if null sourceStack
    then logErrorN ("Tried to pull from empty stack: " <> (pack . show $ s)) >> return crateStacks
    else do
      return $ HM.insert d (take num sourceStack ++ destStack) (HM.insert s (drop num sourceStack) crateStacks)

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution _ = return Nothing

-------------------- SOLUTION PATTERNS --------------------

solveFold :: (MonadLogger m) => CrateStacks -> [Move] -> m EasySolutionType
solveFold = foldM foldLine

type FoldType = CrateStacks

-- initialFoldV :: FoldType
-- initialFoldV = undefined

foldLine :: (MonadLogger m) => FoldType -> Move -> m FoldType
foldLine crateStacks (Move num src dst) = do
  let sourceStack = fromMaybe [] (HM.lookup src crateStacks)
      destStack = fromMaybe [] (HM.lookup dst crateStacks)
  if null sourceStack
    then logErrorN ("Tried to pull from empty stack: " <> (pack . show $ src)) >> return crateStacks
    else do
      return $ HM.insert dst (reverse (take num sourceStack) ++ destStack) (HM.insert src (drop num sourceStack) crateStacks)

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

easySmall :: IO (Maybe String)
easySmall = solveEasy smallFile

easyLarge :: IO (Maybe String)
easyLarge = solveEasy largeFile

hardSmall :: IO (Maybe String)
hardSmall = solveHard smallFile

hardLarge :: IO (Maybe String)
hardLarge = solveHard largeFile