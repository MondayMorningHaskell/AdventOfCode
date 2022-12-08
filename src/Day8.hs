{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1)
import Text.Megaparsec.Char (eol)
import qualified Data.Array as A
import qualified Data.Set as S
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, Grid2, parse2DDigitArray, Coord2, countWhere)
import Control.Monad (foldM, when)

dayNum :: Int
dayNum = 8

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
type InputType = Grid2 Int

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = parse2DDigitArray

-- parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
-- parseInput =
--   sepEndyBy1 parseLine eol

-- type InputType = [LineType]
-- type LineType = ()

-- parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
-- parseLine = return ()

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy treeGrid = do
  let columns = [0..(snd . snd . A.bounds $ treeGrid)]
  s1 <- foldM (countVisibleHorizontal treeGrid columns) S.empty rows
  s2 <- foldM (countVisibleHorizontal treeGrid (reverse columns)) s1 rows
  let rows = [0..(fst . snd . A.bounds $ treeGrid)]
  s3 <- foldM (countVisibleVertical treeGrid rows) s2 cols
  S.size <$> foldM (countVisibleVertical treeGrid (reverse rows)) s3 cols
  where
    rows = [0..(fst . snd . A.bounds $ treeGrid)]
    cols = [0..(snd . snd . A.bounds $ treeGrid)]
-- List all the directions
-- Create a set
-- Fold through all the directions

countVisibleHorizontal :: (MonadLogger m) => Grid2 Int -> [Int] -> S.Set Coord2 -> Int -> m (S.Set Coord2)
countVisibleHorizontal treeGrid columns prev row = return $ fst $ foldl assessColumn (prev, -1) columns
  where
    assessColumn :: (S.Set Coord2, Int) -> Int -> (S.Set Coord2, Int)
    assessColumn (prevSet, highestSeen) col =
      let nextHeight = treeGrid A.! (row, col)
      in  if nextHeight > highestSeen then (S.insert (row, col) prevSet, nextHeight) else (prevSet, highestSeen)

countVisibleVertical :: (MonadLogger m) => Grid2 Int -> [Int] -> S.Set Coord2 -> Int -> m (S.Set Coord2)
countVisibleVertical treeGrid rows prev col = return $ fst $ foldl assessRow (prev, -1) rows
  where
    assessRow :: (S.Set Coord2, Int) -> Int -> (S.Set Coord2, Int)
    assessRow (prevSet, highestSeen) row =
      let nextHeight = treeGrid A.! (row, col)
      in  if nextHeight > highestSeen then (S.insert (row, col) prevSet, nextHeight) else (prevSet, highestSeen)

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard treeGrid = maximum <$> mapM (assessScenicScore treeGrid) (A.indices treeGrid)

assessScenicScore :: (MonadLogger m) => Grid2 Int -> Coord2 -> m HardSolutionType
assessScenicScore treeGrid (row, col) = return $ lookUp * lookLeft * lookDown * lookRight
  where
    heightHere = treeGrid A.! (row, col)
    (maxRow, maxCol) = snd (A.bounds treeGrid)
    lookUp = if row == 0 then 0
      else
        let smallerTrees = length $ takeWhile (\r -> treeGrid A.! (r, col) < heightHere) [(row - 1),(row - 2)..0]
        in  if smallerTrees == row then smallerTrees else smallerTrees + 1
    lookLeft = if col == 0 then 0
      else
        let smallerTrees = length $ takeWhile (\c -> treeGrid A.! (row, c) < heightHere) [(col - 1),(col-2)..0]
        in  if smallerTrees == col then smallerTrees else smallerTrees + 1
    lookDown = if row == maxRow then 0
      else
        let smallerTrees = length $ takeWhile (\r -> treeGrid A.! (r, col) < heightHere) [(row + 1)..maxRow]
        in  if smallerTrees + row == maxRow then smallerTrees else smallerTrees + 1
    lookRight = if col == maxCol then 0
      else
        let smallerTrees = length $ takeWhile (\c -> treeGrid A.! (row, c) < heightHere) [(col + 1)..maxCol]
        in  if smallerTrees + col == maxCol then smallerTrees else smallerTrees + 1

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