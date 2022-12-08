{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Control.Monad (foldM)
import Control.Monad.State (StateT, MonadState (get, put))
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT)
import Data.Maybe (fromMaybe)
import Data.List (tails, sort, find)
import qualified Data.Map as M
import Text.Megaparsec (ParsecT, sepEndBy1, (<|>), some, MonadParsec (try))
import Text.Megaparsec.Char (eol, string, letterChar, char)
import Data.Void (Void)
import Data.Text (Text, pack, unpack)

import Utils (parseFile, parsePositiveNumber, OccMapBig, addKey)

dayNum :: Int
dayNum = 7

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO (Maybe Integer)
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  result <- processInputEasy input
  findEasySolution result

solveHard :: FilePath -> IO (Maybe Integer)
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  result <- processInputEasy input
  findHardSolution result

-------------------- PARSING --------------------
type InputType = [LineType]
type LineType = Command
data Command =
  ChangeDirectoryCommand String |
  ListDirectoryCommand |
  ListedDirectory String |
  ListedFile Integer String
  deriving (Show)

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = sepEndBy1 parseLine eol

parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
parseLine = parseCD <|> parseLS <|> parseDir <|> parseFile
  where
    parseCD = do
      string "$ cd "
      dir <- (unpack <$> string "..") <|> (unpack <$> string "/") <|> some letterChar
      return $ ChangeDirectoryCommand dir
    parseLS = string "$ ls" >> return ListDirectoryCommand
    parseDir = do
      string "dir "
      dir <- some letterChar
      return $ ListedDirectory dir
    parseFile = do
      fileSize <- fromIntegral <$> parsePositiveNumber
      char ' '
      fileName <- some (letterChar <|> char '.')
      return $ ListedFile fileSize fileName

-------------------- SOLVING EASY --------------------
type EasySolutionType = OccMapBig [String]

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy inputs = directoryMap <$> solveFold inputs

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Integer)
findEasySolution dirMap = do
  let largePairs = filter (<= 100000) (M.elems dirMap)
  return $ Just $ sum largePairs

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard _ = undefined

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Integer)
findHardSolution dirMap = do
  let allDirSizes = sort (M.elems dirMap)
  let usedSpace = last allDirSizes
  let currentUnusedSpace = 70000000 - usedSpace
  return $ find (\i -> currentUnusedSpace + i >= 30000000) allDirSizes

-------------------- SOLUTION PATTERNS --------------------

solveFold :: (MonadLogger m) => [LineType] -> m FSState
solveFold = foldM foldLine initialFoldV

data FSState = FSState
  { currentDirectory :: [String]
  , directoryMap :: OccMapBig [String]
  } deriving (Show)

initialFoldV :: FSState
initialFoldV = FSState [] M.empty

foldLine :: (MonadLogger m) => FSState -> LineType -> m FSState
foldLine prevState command = case command of
  ChangeDirectoryCommand dir -> if dir == ".."
    then return $ prevState { currentDirectory = tail (currentDirectory prevState)}
    else if dir == "/"
      then return $ prevState { currentDirectory = ["/"]}
      else return $ prevState { currentDirectory = dir : currentDirectory prevState}
  ListedFile size _ -> do
    let allDirs = currentDirectory prevState
    let newDirMap = foldl (\mp d -> addKey mp d size) (directoryMap prevState) (init $ tails allDirs)
    return $ prevState { directoryMap = newDirMap}
  _ -> return prevState

-------------------- BOILERPLATE --------------------
smallFile :: FilePath
smallFile = "inputs_2022/day_" <> show dayNum <> "_small.txt"

largeFile :: FilePath
largeFile = "inputs_2022/day_" <> show dayNum <> "_large.txt"

easySmall :: IO (Maybe Integer)
easySmall = solveEasy smallFile

easyLarge :: IO (Maybe Integer)
easyLarge = solveEasy largeFile

hardSmall :: IO (Maybe Integer)
hardSmall = solveHard smallFile

hardLarge :: IO (Maybe Integer)
hardLarge = solveHard largeFile