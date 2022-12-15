{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1)
import Text.Megaparsec.Char (eol, string)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, Coord2, parsePositiveNumber, parseSignedInteger, manhattanDistance, getNeighbors4Unbounded, countWhere)
import Control.Monad (foldM, when)
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import Data.List (sort, nub)
import Data.Maybe (catMaybes)

dayNum :: Int
dayNum = 15

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> Int -> IO (Maybe Int)
solveEasy fp size = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  Just <$> processInputEasy input size

solveHard :: FilePath -> Int -> IO (Maybe Integer)
solveHard fp size = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  result <- processInputHard input size
  findHardSolution result

-------------------- PARSING --------------------
parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput =
  sepEndBy1 parseLine eol

type InputType = [LineType]
type LineType = (Coord2, Coord2)

parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
parseLine = do
  string "Sensor at x="
  i <- parseSignedInteger
  string ", y="
  j <- parseSignedInteger
  string ": closest beacon is at x="
  k <- parseSignedInteger
  string ", y="
  l <- parseSignedInteger
  return ((i, j), (k, l))

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> Int -> m EasySolutionType
processInputEasy inputs size = do
  resultingIntervals <- mapM (excludedCoords size) inputs
  mergedIntervals <- mergeIntervals (catMaybes resultingIntervals)
  let beacons = nub $ filter (\c@(_, y) -> y == size) (snd <$> inputs)
  countIntervalsExcludingBeacons mergedIntervals (fst <$> beacons)

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = Maybe Coord2

processInputHard :: (MonadLogger m) => InputType -> Int -> m HardSolutionType
processInputHard inputs maxDimen = evaluateRow 0
  where
    evaluateRow :: (MonadLogger m) => Int -> m (Maybe Coord2)
    evaluateRow row = if row > maxDimen then return Nothing
      else do
        resultingIntervals <- mapM (excludedCoords row) inputs
        mergedIntervals <- mergeIntervals (catMaybes resultingIntervals)
        result <- findHole mergedIntervals maxDimen
        case result of
          Nothing -> evaluateRow (row + 1)
          Just col -> return $ Just (col, row)

findHole :: (MonadLogger m) => [Interval] -> Int -> m (Maybe Int)
findHole [] _ = return Nothing
findHole [(start, end)] maxCol
  | start > 0 = return (Just (start - 1))
  | end < maxCol = return (Just (end + 1))
  | otherwise = return Nothing
findHole ((start1, end1) : (start2, end2) : rest) maxCol = if end1 + 1 < start2 && (end1 + 1) >= 0 && (end1 + 1) <= maxCol
  then return (Just (end1 + 1))
  else findHole ((start2, end2) : rest) maxCol

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Integer)
findHardSolution Nothing = return Nothing
findHardSolution (Just (col, row)) = return $ Just $ fromIntegral col * 4000000 + fromIntegral row

-------------------- SOLUTION PATTERNS --------------------

type Interval = (Int, Int)

excludedCoords :: (MonadLogger m) => Int -> (Coord2, Coord2) -> m (Maybe Interval)
excludedCoords rowNum (sensor@(sx, sy), beacon) = do
  let dist = manhattanDistance sensor beacon
  let distToRow = abs (sy - rowNum)
  let leftoverDist = dist - distToRow
  if leftoverDist < 0
    then return Nothing
    else return $ Just (sx - leftoverDist, sx + leftoverDist)

mergeIntervals :: (MonadLogger m) => [Interval] -> m [Interval]
mergeIntervals [] = return []
mergeIntervals intervals = do
  let sorted = sort intervals
  mergeTail [] (head sorted) (tail sorted)
  where
    mergeTail :: (MonadLogger m) => [Interval] -> Interval -> [Interval] -> m [Interval]
    mergeTail accum current [] = return $ reverse (current : accum)
    mergeTail accum current@(cStart, cEnd) (first@(fStart, fEnd) : rest) = if fStart > cEnd 
      then mergeTail (current : accum) first rest
      else mergeTail accum (cStart, max cEnd fEnd) rest

countIntervalsExcludingBeacons :: (MonadLogger m) => [Interval] -> [Int] -> m Int
countIntervalsExcludingBeacons intervals beaconXs = countTail 0 intervals (sort beaconXs)
  where
    countTail :: (MonadLogger m) => Int -> [Interval] -> [Int] -> m Int
    countTail accum [] _ = return accum
    countTail accum ((next1, next2) : rest) [] = countTail (accum + (next2 - next1 + 1)) rest []
    countTail accum ints@((next1, next2) : restInts) beacons@(nextBeaconX : restBeacons)
      | nextBeaconX < next1 = countTail accum ints restBeacons
      | nextBeaconX > next2 = countTail (accum + (next2 - next1)) restInts restBeacons
      | otherwise = countTail (accum - 1) ints restBeacons

-------------------- BOILERPLATE --------------------
smallFile :: FilePath
smallFile = "inputs_2022/day_" <> show dayNum <> "_small.txt"

largeFile :: FilePath
largeFile = "inputs_2022/day_" <> show dayNum <> "_large.txt"

easySmall :: Int -> IO (Maybe Int)
easySmall = solveEasy smallFile

easyLarge :: Int -> IO (Maybe Int)
easyLarge = solveEasy largeFile

hardSmall :: Int -> IO (Maybe Integer)
hardSmall = solveHard smallFile

hardLarge :: Int -> IO (Maybe Integer)
hardLarge = solveHard largeFile