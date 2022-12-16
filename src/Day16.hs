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
import Algorithm.Search (dijkstraM, aStarM)
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad (forM_)

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

-- parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
-- parseInput =
--   return ()

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = do
  combos <- sepEndBy1 parseLine eol
  return (HM.fromList (fst <$> combos), HM.fromList (snd <$> combos))

-- type InputType = [LineType]
-- type LineType = ()

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
  logErrorN (pack . show $ flowingNodes)
  result <- dijkstraM (getNeighbors (flowValues', distMat)) getCost isFinished initialState
  case result of
    Nothing -> return (-1)
    Just path -> return (pressureReleased . last . snd $ path)
  where
    flowValues' = HM.filter (> 0) flowValues
    flowingNodes = HM.keys flowValues'
    initialState = GraphState "AA" (sum . HM.elems $ flowValues') 0 (HS.fromList flowingNodes) 0 0

getNeighbors :: (MonadLogger m) => (HM.HashMap String Int, HM.HashMap (String, String) Int) -> GraphState -> m [GraphState]
getNeighbors (flowValues, distanceMap) (GraphState currentNode flRate relRate flowing sp released) = do
  let tunnelMoves = mapMaybe tunnelMove (HS.toList flowing)
  -- logErrorN ("Current: " <> (pack . show $ currentNode) <> " Neighbors: " <> (pack . show $ map current tunnelMoves) <> " Step: " <> (pack . show $ sp))
  return tunnelMoves
  where
  -- Move to other locations
    tunnelMove newNode = do
      let dist = 1 + (distanceMap HM.! (currentNode, newNode))
      let newStep = sp + dist
      let flowAtNode = fromMaybe 0 (HM.lookup newNode flowValues)
      if newStep > 30
        then Just $ GraphState newNode (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete newNode flowing) 30 (released + (30 - sp) * relRate)
        else Just $ GraphState newNode (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete newNode flowing) newStep (released + dist * relRate)

getCost :: (MonadLogger m) => GraphState -> GraphState -> m Int
getCost (GraphState _ flRate _ _ oldStep _) (GraphState _ _ _ _ newStep _) = do
  let stepDiff = newStep - oldStep
  return $ flRate * stepDiff

isFinished :: (MonadLogger m) => GraphState -> m Bool
isFinished (GraphState _ _ _ flowing x _) = return (x >= 30 || HS.null flowing)

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
data GraphStateHard = GraphStateHard
  { player :: String
  , elephant :: String
  , flowingRateH :: Int
  , releasedRateH :: Int
  , flowingValuesH :: HS.HashSet String
  , stepH :: Int
  , pressureReleasedH :: Int
  } deriving (Show, Eq, Ord)

data Move =
  MoveNode String | UseValve String

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard inputs@(flowValues, adjacencyMap) = do
  result <- dijkstraM (getNeighborsHard inputs) getCostHard {-(estimateRemainingCost inputs)-} isFinishedHard initialState
  case result of
    Nothing -> return (-1)
    Just path -> do
      forM_ path $ \st -> logErrorN (pack . show $ st)
      return (pressureReleasedH . last . snd $ path)
  where
    flowValues' = HM.filter (> 0) flowValues
    flowingNodes = HM.keys flowValues'
    initialState = GraphStateHard "AA" "AA" (sum . HM.elems $ flowValues') 0 (HS.fromList flowingNodes) 0 0

getNeighborsHard :: (MonadLogger m) => InputType -> GraphStateHard -> m [GraphStateHard]
getNeighborsHard (flowValues, adjacencyMap) (GraphStateHard playerNode elephantNode flRate relRate flowing sp released) = do
  moves <- mapM makeNewState allMoves
  if null moves
    then return [GraphStateHard playerNode elephantNode flRate relRate flowing (sp + 1) released]
    else return moves
  where
    playerValve = [UseValve playerNode | HS.member playerNode flowing]
    playerMoves = playerValve ++ map MoveNode (adjacencyMap HM.! playerNode)

    elephantValve = [UseValve elephantNode | HS.member elephantNode flowing]
    elephantMoves = elephantValve ++ map MoveNode (adjacencyMap HM.! elephantNode)

    allMoves :: [(Move, Move)]
    allMoves = [(p, e) | p <- playerMoves, e <- elephantMoves, isValidCombo p e]

    makeNewState :: (MonadLogger m) => (Move, Move) -> m GraphStateHard
    makeNewState (playerMove, elephantMove) = do
      let (newPlayerNode, playerFlowChange, playerFlowSet, playerReward) = case playerMove of
            MoveNode s -> (s, 0, flowing, 0)
            UseValve s -> (playerNode, flowValues HM.! s, HS.delete s flowing, (26 - sp - 1) * flowValues HM.! s)
      let (newElephantNode, elephantFlowChange, elephantFlowSet, elephantReward) = case elephantMove of
            MoveNode s -> (s, 0, playerFlowSet, 0)
            UseValve s -> (elephantNode, flowValues HM.! s, HS.delete s playerFlowSet, (26 - sp - 1) * flowValues HM.! s)
      let newFlRate = flRate - playerFlowChange - elephantFlowChange
      let newRelRate = relRate + playerFlowChange + elephantFlowChange
      return $ GraphStateHard newPlayerNode newElephantNode newFlRate newRelRate elephantFlowSet (sp + 1) (released + playerReward + elephantReward)

    isValidCombo :: Move -> Move -> Bool
    isValidCombo (UseValve v1) (UseValve v2) = v1 /= v2
    isValidCombo _ _ = True

getCostHard :: (MonadLogger m) => GraphStateHard -> GraphStateHard -> m Int
getCostHard (GraphStateHard _ _ flRate _ _ oldStep _) (GraphStateHard _ _ _ _ _ newStep _ ) = return $ (newStep - oldStep) * flRate

estimateRemainingCost :: (MonadLogger m) => InputType -> GraphStateHard -> m Int
estimateRemainingCost (flowValues, _) (GraphStateHard playerNode elephantNode flRate _ flowing sp _) = do
  let estimatedFlow = flRate - currentPlayerFlow - currentElephantFlow
  return $ estimatedFlow * (26 - sp)
  where
    currentPlayerFlow = if HS.member playerNode flowing then flowValues HM.! playerNode else 0
    currentElephantFlow = if HS.member elephantNode flowing then flowValues HM.! elephantNode else 0

isFinishedHard :: (MonadLogger m) => GraphStateHard -> m Bool
isFinishedHard (GraphStateHard _ _ _ _ _ sp _) = return (sp >= 26)

type HardSolutionType = EasySolutionType

--   let distMat = allNodesShortestPath adjacencyMap
--   logErrorN (pack . show $ flowingNodes)
--   result <- dijkstraM (getNeighborsHard (flowValues', distMat)) getCostHard isFinishedHard initialState
--   case result of
--     Nothing -> return (-1)
--     Just path -> return (pressureReleasedH . last . snd $ path)
--   where
--     flowValues' = HM.filter (> 0) flowValues
--     flowingNodes = HM.keys flowValues'
--     initialState = GraphStateHard ("AA", 0) ("AA", 0) (sum . HM.elems $ flowValues') 0 (HS.fromList flowingNodes) 0 0

-- getNeighborsHard :: (MonadLogger m) => (HM.HashMap String Int, HM.HashMap (String, String) Int) -> GraphStateHard -> m [GraphStateHard]
-- getNeighborsHard (flowValues, distanceMap) (GraphStateHard (playerNode, playerDist) (elephantNode, elephantDist) flRate relRate flowing sp released)
--   | HS.null flowing = noMoreMoves
--   | playerDist == 0 && elephantDist == 0 = pickMovesBoth
--   | playerDist == 0 = pickMovesPlayer
--   | elephantDist == 0 = pickMovesElephant
--   | otherwise = error "This can't happen"
--   where
--     noMoreMoves
--       | playerDist == 0 && elephantDist == 0 =
--         return [GraphStateHard (playerNode, 0) (elephantNode, 0) flRate relRate flowing 26 (released + (26 - sp) * relRate)]
--       -- | playerDist == 0 = do
--       --   let newStep = sp + elephantDist
--       --   let flowAtNode = fromMaybe 0 (HM.lookup elephantNode flowValues)
--       --   if newStep > 26
--       --     then return [GraphStateHard (playerNode, 0) (elephantNode, elephantDist - (26 - sp)) flRate relRate flowing 26 (released + (26 - sp) * relRate)]
--       --     else return [GraphStateHard (playerNode, 0) (elephantNode, 0) (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete elephantNode flowing) newStep (released + relRate * elephantDist)]
--       -- | elephantDist == 0 = do
--       --   let newStep = sp + playerDist
--       --   let flowAtNode = fromMaybe 0 (HM.lookup playerNode flowValues)
--       --   if newStep > 26
--       --     then return [GraphStateHard (playerNode, playerDist - (26 - sp)) (elephantNode, 0) flRate relRate flowing 26 (released + (26 - sp) * relRate)]
--       --     else return [GraphStateHard (playerNode, 0) (elephantNode, 0) (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete playerNode flowing) newStep (released + relRate * elephantDist)]
--       | otherwise = error "This can't happen"

--     -- Player is 0 and elephant is not 0
--     --   Player must select a new move.
--     --   For each possible new location, we get a new state. Either the player or elephant (or both) reaches the location.
--     pickMovesPlayer :: (MonadLogger m) => m [GraphStateHard]
--     pickMovesPlayer = do
--       let possibleMoves = [n | n <- HS.toList flowing, n /= elephantNode] -- TODO: Perhaps I shouldn't allow n == elephant node because it gets confused.
--       mapM playerMove possibleMoves
    
--     playerMove :: (MonadLogger m) => String -> m GraphStateHard
--     playerMove newPlayerNode = do
--       let playerDist = 1 + (distanceMap HM.! (playerNode, newPlayerNode))
--       if playerDist < elephantDist
--         then do
--           let newStep = sp + playerDist
--           let flowAtNode = fromMaybe 0 (HM.lookup newPlayerNode flowValues)
--           if newStep > 26
--             then return $ GraphStateHard (newPlayerNode, 0) (elephantNode, elephantDist - playerDist) (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete newPlayerNode flowing) 26 (released + (26 - sp) * relRate)
--             else return $ GraphStateHard (newPlayerNode, 0) (elephantNode, elephantDist - playerDist) (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete newPlayerNode flowing) newStep (released + playerDist * relRate)
--         else if elephantDist < playerDist
--           then do
--             let newStep = sp + elephantDist
--             let flowAtNode = fromMaybe 0 (HM.lookup elephantNode flowValues)
--             if newStep > 26
--               then return $ GraphStateHard (newPlayerNode, playerDist - elephantDist) (elephantNode, 0) (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete elephantNode flowing) 26 (released + (26 - sp) * relRate)
--               else return $ GraphStateHard (newPlayerNode, playerDist - elephantDist) (elephantNode, 0) (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete elephantNode flowing) newStep (released + elephantDist * relRate)
--           else do
--             let newStep = sp + playerDist
--             let combinedFlows = fromMaybe 0 (HM.lookup newPlayerNode flowValues) + fromMaybe 0 (HM.lookup elephantNode flowValues)
--             let newSet = HS.delete newPlayerNode (HS.delete elephantNode flowing)
--             if newStep > 26
--               then return $ GraphStateHard (newPlayerNode, 0) (elephantNode, 0) (flRate - combinedFlows) (relRate + combinedFlows) newSet 26 (released + (26 - sp) * relRate)
--               else return $ GraphStateHard (newPlayerNode, 0) (elephantNode, 0) (flRate - combinedFlows) (relRate + combinedFlows) newSet newStep (released + elephantDist * relRate)

--     -- Elephant is 0 and player is not 0
--     --   Same as above, but reversed
--     pickMovesElephant = do
--       let possibleMoves = [n | n <- HS.toList flowing, n /= playerNode]
--       mapM elephantMove possibleMoves

--     elephantMove :: (MonadLogger m) => String -> m GraphStateHard
--     elephantMove newElephantNode = do
--       let elephantDist = 1 + (distanceMap HM.! (elephantNode, newElephantNode))
--       if playerDist < elephantDist
--         then do
--           let newStep = sp + playerDist
--           let flowAtNode = fromMaybe 0 (HM.lookup playerNode flowValues)
--           if newStep > 26
--             then return $ GraphStateHard (playerNode, 0) (newElephantNode, elephantDist - playerDist) (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete playerNode flowing) 26 (released + (26 - sp) * relRate)
--             else return $ GraphStateHard (playerNode, 0) (newElephantNode, elephantDist - playerDist) (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete playerNode flowing) newStep (released + playerDist * relRate)
--         else if elephantDist < playerDist
--           then do
--             let newStep = sp + elephantDist
--             let flowAtNode = fromMaybe 0 (HM.lookup newElephantNode flowValues)
--             if newStep > 26
--               then return $ GraphStateHard (playerNode, playerDist - elephantDist) (newElephantNode, 0) (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete newElephantNode flowing) 26 (released + (26 - sp) * relRate)
--               else return $ GraphStateHard (playerNode, playerDist - elephantDist) (newElephantNode, 0) (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete newElephantNode flowing) newStep (released + elephantDist * relRate)
--           else do
--             let newStep = sp + playerDist
--             let combinedFlows = fromMaybe 0 (HM.lookup playerNode flowValues) + fromMaybe 0 (HM.lookup newElephantNode flowValues)
--             let newSet = HS.delete playerNode (HS.delete newElephantNode flowing)
--             if newStep > 26
--               then return $ GraphStateHard (playerNode, 0) (newElephantNode, 0) (flRate - combinedFlows) (relRate + combinedFlows) newSet 26 (released + (26 - sp) * relRate)
--               else return $ GraphStateHard (playerNode, 0) (newElephantNode, 0) (flRate - combinedFlows) (relRate + combinedFlows) newSet newStep (released + elephantDist * relRate)
    
--     -- Player and Elephant are both 0
--     --   Both select new moves (that are different).
--     --   Within each 'pair' of states, the smaller one (or both) reaches the destination.
--     pickMovesBoth = do
--       let movePairs = [(s1, s2) | s1 <- HS.toList flowing, s2 <- HS.toList flowing, s1 /= s2]
--       mapM moveBoth movePairs
    
--     moveBoth :: (MonadLogger m) => (String, String) -> m GraphStateHard
--     moveBoth (newPlayerNode, newElephantNode) = do
--       let playerDist = 1 + (distanceMap HM.! (playerNode, newPlayerNode))
--       let elephantDist = 1 + (distanceMap HM.! (elephantNode, newElephantNode))
--       if playerDist < elephantDist
--         then do
--           let newStep = sp + playerDist
--           let flowAtNode = fromMaybe 0 (HM.lookup newPlayerNode flowValues)
--           if newStep > 26
--             then return $ GraphStateHard (newPlayerNode, 0) (newElephantNode, elephantDist - playerDist) (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete newPlayerNode flowing) 26 (released + (26 - sp) * relRate)
--             else return $ GraphStateHard (newPlayerNode, 0) (newElephantNode, elephantDist - playerDist) (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete newPlayerNode flowing) newStep (released + playerDist * relRate)
--         else if elephantDist < playerDist
--           then do
--             let newStep = sp + elephantDist
--             let flowAtNode = fromMaybe 0 (HM.lookup newElephantNode flowValues)
--             if newStep > 26
--               then return $ GraphStateHard (newPlayerNode, playerDist - elephantDist) (newElephantNode, 0) (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete newElephantNode flowing) 26 (released + (26 - sp) * relRate)
--               else return $ GraphStateHard (newPlayerNode, playerDist - elephantDist) (newElephantNode, 0) (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete newElephantNode flowing) newStep (released + elephantDist * relRate)
--           else do
--             let newStep = sp + playerDist
--             let combinedFlows = fromMaybe 0 (HM.lookup newPlayerNode flowValues) + fromMaybe 0 (HM.lookup newElephantNode flowValues)
--             let newSet = HS.delete newPlayerNode (HS.delete newElephantNode flowing)
--             if newStep > 26
--               then return $ GraphStateHard (newPlayerNode, 0) (newElephantNode, 0) (flRate - combinedFlows) (relRate + combinedFlows) newSet 26 (released + (26 - sp) * relRate)
--               else return $ GraphStateHard (newPlayerNode, 0) (newElephantNode, 0) (flRate - combinedFlows) (relRate + combinedFlows) newSet newStep (released + elephantDist * relRate)

--   -- let tunnelMoves = mapMaybe tunnelMove (HS.toList flowing)
--   -- -- logErrorN ("Current: " <> (pack . show $ currentNode) <> " Neighbors: " <> (pack . show $ map current tunnelMoves) <> " Step: " <> (pack . show $ sp))
--   -- return tunnelMoves
--   -- where
--   -- -- Move to other locations
--   --   tunnelMove newNode = do
--   --     let dist = 1 + (distanceMap HM.! (currentNode, newNode))
--   --     let newStep = sp + dist
--   --     let flowAtNode = fromMaybe 0 (HM.lookup newNode flowValues)
--   --     if newStep > 30
--   --       then Just $ GraphState newNode (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete newNode flowing) 30 (released + (30 - sp) * relRate)
--   --       else Just $ GraphState newNode (flRate - flowAtNode) (relRate + flowAtNode) (HS.delete newNode flowing) newStep (released + dist * relRate)

-- getCostHard :: (MonadLogger m) => GraphStateHard -> GraphStateHard -> m Int
-- getCostHard (GraphStateHard _ _ flRate _ _ oldStep _) (GraphStateHard _ _ _ _ _ newStep _) = do
--   let stepDiff = newStep - oldStep
--   return $ flRate * stepDiff

-- isFinishedHard :: (MonadLogger m) => GraphStateHard -> m Bool
-- isFinishedHard (GraphStateHard _ _ _ _ _ x _) = return (x >= 26)

-- findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
-- findHardSolution _ = return Nothing

-------------------- SOLUTION PATTERNS --------------------

distance :: String -> String -> Int
distance = undefined

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