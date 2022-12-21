{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day19 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1)
import Text.Megaparsec.Char (eol, string)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, parsePositiveNumber)
import Control.Monad (foldM, when, forM_, forM)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Maybe (catMaybes)
import Algorithm.Search (dijkstraM, aStarM)
import Control.Applicative ((<|>))

dayNum :: Int
dayNum = 19

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
type InputType = [LineType]
type LineType = BluePrint

data BluePrint = BluePrint
  { idNumber :: Int
  , oreRobotCost :: Int
  , clayRobotCost :: Int
  , obsidianRobotCost :: (Int, Int)
  , geodeRobotCost :: (Int, Int)
  } deriving (Show)

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = sepEndBy1 parseLine eol

parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
parseLine = do
  string "Blueprint "
  bpNum <- parsePositiveNumber
  string ": Each ore robot costs "
  oreCost <- parsePositiveNumber
  string " ore. Each clay robot costs "
  clayCost <- parsePositiveNumber
  string " ore. Each obsidian robot costs "
  obsOreCost <- parsePositiveNumber
  string " ore and "
  obsClayCost <- parsePositiveNumber
  string " clay. Each geode robot costs "
  geodeOreCost <- parsePositiveNumber
  string " ore and "
  geodeObsCost <- parsePositiveNumber
  string " obsidian."
  return $ BluePrint bpNum oreCost clayCost (obsOreCost, obsClayCost) (geodeOreCost, geodeObsCost)

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy = foldM foldLine initialFoldV

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard blueprints = foldM foldLineHard initialFoldH (take 3 blueprints)

initialFoldH :: FoldType
initialFoldH = 1

foldLineHard :: (MonadLogger m) => FoldType -> LineType -> m FoldType
foldLineHard prev blueprint = do
  quality <- fst <$> dfs 32 blueprint (0, Set.empty) [initialState]
  return $ prev * quality
  where
    initialState = SearchState 1 0 0 0 0 0 0 0 0

-------------------- SOLUTION PATTERNS --------------------

type FoldType = Int

initialFoldV :: FoldType
initialFoldV = 0

foldLine :: (MonadLogger m) => FoldType -> LineType -> m FoldType
foldLine prev blueprint = do
  quality <- fst <$> dfs 24 blueprint (0, Set.empty) [initialState]
  return $ prev + (idNumber blueprint * quality)
  where
    initialState = SearchState 1 0 0 0 0 0 0 0 0

data SearchState = SearchState
  { numOreRobots :: Int
  , numClayRobots :: Int
  , numObsidianRobots :: Int
  , numGeodeRobots :: Int
  , ore :: Int
  , clay :: Int
  , obsidian :: Int
  , geodes :: Int
  , time :: Int
  } deriving (Eq, Ord, Show)

neighbors :: (MonadLogger m) => Int -> Int -> BluePrint -> SearchState -> m [SearchState]
neighbors maxTime prevMax (BluePrint _ o c (obsOre, obsClay) (geoOre, geoObs)) st@(SearchState oRobots cRobots obsRobots geoRobots ore' clay' obsidian' geodes' t) =
  if maxGeodes < prevMax
    then return []
    else do
      let (results :: [SearchState]) = reverse (stepTime : catMaybes [tryMakeOre, tryMakeClay, tryMakeObsidian, tryMakeGeode])
      return results
  where
    stepTime = SearchState oRobots cRobots obsRobots geoRobots (ore' + oRobots) (clay' + cRobots) (obsidian' + obsRobots) (geodes' + geoRobots) (t + 1)
    tryMakeOre = if ore' >= o && oRobots < maximum [o, c, obsOre, geoOre]
      then Just $ stepTime {numOreRobots = oRobots + 1, ore = ore stepTime - o}
      else Nothing
    tryMakeClay = if ore' >= c && cRobots < obsClay
      then Just $ stepTime {numClayRobots = cRobots + 1, ore = ore stepTime - c}
      else Nothing
    tryMakeObsidian = if ore' >= obsOre && clay' >= obsClay && obsRobots < geoObs
      then Just $ stepTime {numObsidianRobots = obsRobots + 1, ore = ore stepTime - obsOre, clay = clay stepTime - obsClay}
      else Nothing
    tryMakeGeode = if ore' >= geoOre && obsidian' >= geoObs
      then Just $ stepTime {numGeodeRobots = geoRobots + 1, ore = ore stepTime - geoOre, obsidian = obsidian stepTime - geoObs}
      else Nothing
    maxGeodes = geodes' + (geoRobots * (maxTime - t)) + sum [1..(maxTime - t)]

dfs :: (MonadLogger m) => Int -> BluePrint -> (Int, Set.Set SearchState) -> [SearchState] -> m (Int, Set.Set SearchState)
dfs maxTime bp (mostGeodes, visited) stack = case stack of
  [] -> return (mostGeodes, visited)
  (top : rest) -> if time top >= maxTime
    then return (max mostGeodes (geodes top), Set.insert top visited)
    else do
      next <- neighbors maxTime mostGeodes bp top
      let next' = filter (\st -> not (st `Set.member` visited)) next
          newVisited = foldl (flip Set.insert) visited next'
      foldM f (mostGeodes, newVisited) next'
  where
    f (prevMax, newVisited) st = do
      (resultVal, visited') <- dfs maxTime bp (prevMax, newVisited) (st : stack)
      return (max resultVal prevMax, visited')

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