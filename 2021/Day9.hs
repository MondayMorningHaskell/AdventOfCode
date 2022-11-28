{-# LANGUAGE OverloadedStrings #-}

module Day9 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Data.Array (Array)
import qualified Data.Array as A
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (sort)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.Megaparsec ()
import Text.Megaparsec.Char ()
import Data.Void (Void)
import Data.Text (Text)
import Utils (parse2DDigitArray, parseFile, Coord2, Grid2, getNeighborsAssoc, getAssocs)

d9ES :: IO (Maybe Int)
d9ES = solveDay9Easy "inputs/day_9_small.txt"

d9EB :: IO (Maybe Int)
d9EB = solveDay9Easy "inputs/day_9_big.txt"

d9HS :: IO (Maybe Int)
d9HS = solveDay9Hard "inputs/day_9_small.txt"

d9HB :: IO (Maybe Int)
d9HB = solveDay9Hard "inputs/day_9_big.txt"

solveDay9Easy :: String -> IO (Maybe Int)
solveDay9Easy fp = do
  inputArray <- parseFile parse2DDigitArray fp
  sinks <- runStdoutLoggingT $ findSinks inputArray
  return $ Just $ sum ((+ 1) . snd <$> sinks)

solveDay9Hard :: String -> IO (Maybe Int)
solveDay9Hard fp = do
  inputArray <- parseFile parse2DDigitArray fp
  basinSizes <- runStdoutLoggingT $ do
    sinks <- runStdoutLoggingT $ findSinks inputArray
    mapM (findBasinSize inputArray) (fst <$> sinks)
  return $ Just (product (take 3 (reverse . sort $ basinSizes)))

findSinks :: (MonadLogger m) => Grid2 Int -> m [(Coord2, Int)]
findSinks input = do
  return $ getAssocs input sinks
  where
    sinks = filter (isSink input) (A.indices input)

isSink :: Grid2 Int -> Coord2 -> Bool
isSink input coord = all isHigher neighbors
  where
    thisIndex = input A.! coord
    neighbors = getNeighborsAssoc input coord

    isHigher :: (Coord2, Int) -> Bool
    isHigher (_, val) = val > thisIndex

findBasinSize :: (MonadLogger m) => Grid2 Int -> Coord2 -> m Int
findBasinSize grid sink = findBasinSizeTail (Seq.singleton sink, HS.empty, 0)
  where
    findBasinSizeTail :: (MonadLogger m) => (Seq Coord2, HashSet Coord2, Int) -> m Int
    findBasinSizeTail (queue, visited, count) = case Seq.viewl queue of
      Seq.EmptyL -> return count
      top Seq.:< rest -> if HS.member top visited
        then findBasinSizeTail (rest, visited, count)
        else do
          let newVisited = HS.insert top visited
              allNeighbors = getNeighborsAssoc grid top
              neighborsInBasin = filter (\(_, val) -> val < 9) allNeighbors
              unvisitedNeighbors = filter (\c -> not (HS.member c newVisited)) (fst <$> neighborsInBasin)
              newQueue = foldl (Seq.|>) rest unvisitedNeighbors
          findBasinSizeTail (newQueue, newVisited, count + 1)
