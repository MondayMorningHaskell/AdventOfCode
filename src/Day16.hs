{-# LANGUAGE OverloadedStrings #-}

module Day16 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN, logError)
import Text.Megaparsec (ParsecT, sepEndBy1, some, sepBy1, (<|>))
import Text.Megaparsec.Char (eol, letterChar, string)
import Data.Array ( Array, (!), array, assocs, bounds )
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, parsePositiveNumber)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Set as Set
import Algorithm.Search (dijkstraM, aStarM)
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad (forM_, forM, foldM)

dayNum :: Int
dayNum = 16

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO (Maybe Int)
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  -- logErrorN (pack . show $ input)
  Just <$> processInputEasy input

solveHard :: FilePath -> IO (Maybe Int)
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  Just <$> processInputHard input

-------------------- PARSING --------------------
type InputType = (HM.HashMap String Int, HM.HashMap String [String])

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = do
  combos <- sepEndBy1 parseLine eol
  return (HM.fromList (fst <$> combos), HM.fromList (snd <$> combos))

parseLine :: (MonadLogger m) => ParsecT Void Text m ((String, Int), (String, [String]))
parseLine = do
  string "Valve "
  nodeName <- some letterChar
  string " has flow rate="
  i <- parsePositiveNumber
  string "; tunnel leads to valve " <|> string "; tunnels lead to valves "
  neighbors <- sepBy1 (some letterChar) (string ", ")
  return ((nodeName, i), (nodeName, neighbors))

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

data GraphState = GraphState
  { current :: String
  , flowingRate :: Int
  , releasedRate :: Int
  , flowingValues :: HS.HashSet String
  , step :: Int
  , pressureReleased :: Int
  } deriving (Show, Eq, Ord)

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy (flowValues, adjacencyMap) = do
  let distMat = allNodesShortestPath adjacencyMap
  singleActorSearch (flowValues, distMat) (HS.fromList flowingNodes) 30
  where
    flowValues' = HM.filter (> 0) flowValues
    flowingNodes = HM.keys flowValues'

type DistanceMap = HM.HashMap (String, String) Int
type FlowMap = HM.HashMap String Int

singleActorSearch :: (MonadLogger m) => (FlowMap, DistanceMap) -> HS.HashSet String -> Int -> m Int
singleActorSearch (flowValues, distMat) chosenFlowNodes timeLimit = do
  result <- dijkstraM (getNeighbors (flowValues', distMat) timeLimit) getCost (isFinished timeLimit) initialState
  case result of
    Nothing -> return ((maxBound `quot` 2) - 1)
    Just path -> return (pressureReleased . last . snd $ path)
  where
    flowValues' = HM.filter (> 0) flowValues
    initialState = GraphState "AA" (sum . HM.elems $ flowValues') 0 chosenFlowNodes 0 0

getNeighbors :: (MonadLogger m) => (HM.HashMap String Int, HM.HashMap (String, String) Int) -> Int -> GraphState -> m [GraphState]
getNeighbors (flowValues, distanceMap) timeLimit (GraphState currentNode flRate relRate flowing sp released) = do
  let tunnelMoves = mapMaybe tunnelMove (HS.toList flowing)
  if null tunnelMoves
    then do
      let diff = timeLimit - sp
      return [GraphState currentNode flRate relRate flowing timeLimit (released + diff * relRate)]
    else return tunnelMoves
  where
  -- Move to other locations
    tunnelMove newNode = do
      let dist = 1 + (distanceMap HM.! (currentNode, newNode))
      let newStep = sp + dist
      let flowAtNode = fromMaybe 0 (HM.lookup newNode flowValues)
      if newStep > timeLimit
        then Just $ GraphState newNode (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete newNode flowing) timeLimit (released + (timeLimit - sp) * relRate)
        else Just $ GraphState newNode (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete newNode flowing) newStep (released + dist * relRate)

getCost :: (MonadLogger m) => GraphState -> GraphState -> m Int
getCost (GraphState _ flRate _ _ oldStep _) (GraphState _ _ _ _ newStep _) = do
  let stepDiff = newStep - oldStep
  return $ flRate * stepDiff

isFinished :: (MonadLogger m) => Int -> GraphState -> m Bool
isFinished timeLimit (GraphState _ _ _ flowing x _) = return (x >= timeLimit)

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard inputs@(flowValues, adjacencyMap) = snd <$>
  foldM process (1, -1) (Set.toList $ Set.powerSet flowingNodes)
  where
    flowValues' = HM.filter (> 0) flowValues
    flowingNodes = Set.fromList $ HM.keys flowValues'
    distMat = allNodesShortestPath adjacencyMap

    process (iterationNum, prevMax) playerNodes = do
      let elephantNodes = Set.difference flowingNodes playerNodes
      let numPlayer = Set.size playerNodes
      let numElephant = Set.size elephantNodes
      if numPlayer < numElephant
        then return (iterationNum, prevMax)
        else do
          playerResults <- singleActorSearch (flowValues, distMat) (HS.fromList $ Set.toList playerNodes) 26
          elephantResults <- singleActorSearch (flowValues, distMat) (HS.fromList $ Set.toList elephantNodes) 26
          logErrorN ("Running iteration: " <> (pack . show $ iterationNum) <> " " <> (pack . show $ playerResults) <> " " <> (pack . show $ elephantResults))
          return (iterationNum + 1, max prevMax (playerResults + elephantResults))

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

-- Floyd Warshall

allNodesShortestPath :: HM.HashMap String [String] -> HM.HashMap (String, String) Int
allNodesShortestPath adjacencyMap = HM.fromList final
  where
    numKeys = length keys
    keys = HM.keys adjacencyMap
    indices = [0,1..(numKeys - 1)]

    iToS :: HM.HashMap Int String
    iToS = HM.fromList (zip indices keys)

    sToI :: HM.HashMap String Int
    sToI = HM.fromList (zip keys indices)

    initial :: [((String, String), Distance)]
    initial = [((s1, s2), dist s1 s2) | s1 <- keys, s2 <- keys]

    initialM :: [((Int, Int), Distance)]
    initialM = map (\((s1, s2), d) -> ((sToI HM.! s1, sToI HM.! s2), d)) initial

    initialDM :: DistanceMatrix
    initialDM = array ((0, 0), (numKeys - 1, numKeys - 1)) initialM

    finalDM :: DistanceMatrix
    finalDM = floydWarshall initialDM

    finalM :: [((Int, Int), Distance)]
    finalM = assocs finalDM

    final :: [((String, String), Int)]
    final = map (\((i1, i2), d) -> ((iToS HM.! i1, iToS HM.! i2), distToInt d)) finalM

    dist s1 s2
      | s1 == s2 = Distance 0
      | s2 `elem` (adjacencyMap HM.! s1) = Distance 1
      | otherwise = Infinite

type DistanceMatrix = Array (Int, Int) Distance

data Distance = Distance Int | Infinite
  deriving (Show, Eq)

instance Ord Distance where
  compare Infinite Infinite = EQ
  compare _ Infinite = LT
  compare Infinite _ = GT
  compare (Distance d1) (Distance d2) = compare d1 d2

distToInt (Distance i) = i
distToInt Infinite = (maxBound `quot` 2) - 1

addDistance :: Distance -> Distance -> Distance
addDistance Infinite _ = Infinite
addDistance _ Infinite = Infinite
addDistance (Distance d1) (Distance d2) = Distance (d1 + d2)

floydWarshall :: DistanceMatrix -> DistanceMatrix
floydWarshall input = foldl fwFold input [minRow..maxRow]
  where
    (minBounds@(minRow, minCol), maxBounds@(maxRow, maxCol)) = bounds input

    fwFold :: DistanceMatrix -> Int -> DistanceMatrix
    fwFold oldMatrix intermediate =
      let pairs = assocs oldMatrix
      in  array (minBounds, maxBounds) (map (f oldMatrix intermediate) pairs)

    f :: DistanceMatrix -> Int -> ((Int, Int), Distance) -> ((Int, Int), Distance)
    f mat intermediate ((r, c), dist) =
      let altDistance = addDistance (mat ! (r, intermediate)) (mat ! (intermediate, c))
      in  if altDistance < dist then ((r, c), altDistance) else ((r, c), dist)