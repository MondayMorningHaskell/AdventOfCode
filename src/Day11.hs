{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import Control.Monad (forM_)
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Data.List (sort)
import Data.List.Extra (groupOn)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.Megaparsec ()
import Text.Megaparsec.Char ()
import Data.Void (Void)
import Data.Text (Text, pack, concat)
import Utils (parseFile, parse2DDigitHashMap, Coord2, getNeighbors8)

d11ES :: IO (Maybe Int)
d11ES = solveDay11Easy "inputs/day_11_small.txt"

d11EB :: IO (Maybe Int)
d11EB = solveDay11Easy "inputs/day_11_big.txt"

d11HS :: IO (Maybe Int)
d11HS = solveDay11Hard "inputs/day_11_small.txt"

d11HB :: IO (Maybe Int)
d11HB = solveDay11Hard "inputs/day_11_big.txt"

solveDay11Easy :: String -> IO (Maybe Int)
solveDay11Easy fp = do
  initialGrid <- parseFile parse2DDigitHashMap fp
  (_, numFlashes) <- runStdoutLoggingT $ runStepCount 100 (initialGrid, 0)
  return $ Just numFlashes

solveDay11Hard :: String -> IO (Maybe Int)
solveDay11Hard fp = do
  initialGrid <- parseFile parse2DDigitHashMap fp
  firstAllFlash <- runStdoutLoggingT $ runTillAllFlash initialGrid 1
  return $ Just firstAllFlash

type OGrid = HashMap Coord2 Int

runStepCount :: MonadLogger m => Int -> (OGrid, Int) -> m (OGrid, Int)
runStepCount 0 results = return results
runStepCount i (grid, prevFlashes) = do
  (newGrid, flashCount, _) <- runStep grid
  runStepCount (i - 1) (newGrid, flashCount + prevFlashes)

runTillAllFlash :: (MonadLogger m) => OGrid -> Int -> m Int
runTillAllFlash inputGrid thisStep = do
  (newGrid, _, allFlashed) <- runStep inputGrid
  if allFlashed
    then return thisStep
    else runTillAllFlash newGrid (thisStep + 1)

runStep :: (MonadLogger m) => OGrid -> m (OGrid, Int, Bool)
runStep inputGrid = do
  (allFlashes, newGrid) <- processFlashes (HS.fromList initialFlashes) (Seq.fromList initialFlashes) incrementedGrid
  let numFlashes = HS.size allFlashes
  let finalGrid = foldl (\g c -> HM.insert c 0 g) newGrid allFlashes
  return (finalGrid, numFlashes, numFlashes == HM.size inputGrid)
  where
    incrementedGrid = (+1) <$> inputGrid
    initialFlashes = fst <$> filter (\(_, x) -> x >= 10) (HM.toList incrementedGrid)

processFlashes :: (MonadLogger m) => HashSet Coord2 -> Seq Coord2 -> OGrid -> m (HashSet Coord2, OGrid)
processFlashes visited queue grid = case Seq.viewl queue of
  Seq.EmptyL -> return (visited, grid)
  top Seq.:< rest -> do
    let allNeighbors = getNeighbors8 grid top
        newGrid = foldl (\g c -> HM.insert c (g HM.! c + 1) g) grid allNeighbors
        neighborsToAdd = filter shouldAdd allNeighbors
        newVisited = foldl (flip HS.insert) visited neighborsToAdd
        newQueue = foldl (Seq.|>) rest neighborsToAdd
    processFlashes newVisited newQueue newGrid
  where
    shouldAdd :: Coord2 -> Bool
    shouldAdd coord = grid HM.! coord >= 9 && not (HS.member coord visited)

logMap :: (MonadLogger m) => OGrid -> m ()
logMap grid = do
  logDebugN "Start Grid"
  forM_ groupedEntries $ \row -> do
    let asT = fmap (pack . show) (snd <$> row)
    logDebugN (Data.Text.concat asT)
  logDebugN "End Grid"
  logDebugN ""
  where
    sortedEntries = sort $ HM.toList grid
    groupedEntries = groupOn (fst . fst) sortedEntries
