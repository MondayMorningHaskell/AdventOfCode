{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1, MonadParsec (observing))
import Text.Megaparsec.Char (eol)
import qualified Data.Array as A
import Data.List (find)
import Data.Void (Void)
import Data.Text (Text, pack)
import Algorithm.Search (bfsM)

import Utils (parseFile, parse2DCharacterArray, Coord2, Grid2, getNeighbors)
import Control.Monad.Cont (lift)
import Data.Char (ord)

dayNum :: Int
dayNum = 12

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
type InputType = (Grid2 Char, Coord2, Coord2)

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = do
  charArray <- parse2DCharacterArray
  lift $ postProcessGrid charArray

postProcessGrid :: (MonadLogger m) => Grid2 Char -> m InputType
postProcessGrid parsedChars = do
  let allAssocs = A.assocs parsedChars
      start = find (\(c, v) -> v == 'S') allAssocs
      end = find (\(c, v) -> v == 'E') allAssocs
  case (start, end) of
    (Just (s, _), Just (e, _)) -> do
      let newGrid = parsedChars A.// [(s, 'a'), (e, 'z')]
      return (newGrid, s, e)
    _ -> logErrorN "Couldn't find start or end!" >> return (parsedChars, (0, 0), (0, 0))

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
processInputEasy (parsedGrid, start, end) = do
  result <- bfsM (validMoves parsedGrid) (\c -> return (c == end)) start
  case result of
    Nothing -> return maxBound
    Just path -> return (length path)

validMoves :: (MonadLogger m) => Grid2 Char -> Coord2 -> m [Coord2]
validMoves grid current = do
  let neighbors = getNeighbors grid current
      currentHeight = grid A.! current
  return $ filter (neighborTest currentHeight) neighbors
  where
    neighborTest currentHeight newCoord =
      let newHeight = grid A.! newCoord
      in  ord newHeight - ord currentHeight <= 1


findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard (parsedGrid, _, end) = do
  let allStarts = fst <$> filter (\(_, h) -> h == 'a') (A.assocs parsedGrid)
  results <- mapM (\start -> processInputEasy (parsedGrid, start, end)) allStarts
  return $ minimum results

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

hardSmall :: IO (Maybe Int)
hardSmall = solveHard smallFile

hardLarge :: IO (Maybe Int)
hardLarge = solveHard largeFile