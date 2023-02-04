{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day17 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1, (<|>), some)
import Text.Megaparsec.Char (eol, char)
import qualified Data.Array as A
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, Coord2)
import Control.Monad.Cont (lift)
import qualified Data.HashSet as HS
import Control.Monad.State
import Data.List (nub)
import Data.Maybe (mapMaybe)

dayNum :: Int
dayNum = 17

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO (Maybe Integer)
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  Just <$> processInputEasy input

solveHard :: FilePath -> IO (Maybe Integer)
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  Just <$> processInputHard input

-------------------- PARSING --------------------
data Direction = PushLeft | PushRight
  deriving (Show)
type InputType = [Direction]

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = some direction
  where
    direction = (char '>' >> return PushRight) <|> (char '<' >> return PushLeft)

-------------------- SOLVING EASY --------------------
type EasySolutionType = Integer

data BlockType = Minus | Plus | Ell | Long | Square
  deriving (Show)

data Block = Block
  { blockType :: BlockType
  , leftPosition :: Int
  , lowestPosition :: Int
  } deriving (Show)

processInputEasy :: (MonadLogger m, MonadFail m) => InputType -> m EasySolutionType
processInputEasy directions = evalStateT runTetris (initialTetrisState directions 2022)

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m, MonadFail m) => InputType -> m HardSolutionType
processInputHard directions = do
  finalState <- execStateT runTetris (initialTetrisState directions 100000)
  let diffs = reverse $ heightDiffs finalState
  results <- findCycle (A.listArray (1, length diffs) diffs)
  case results of
    Nothing -> return (-1)
    Just (startIndex, cycleLength, sumCycle) -> do
      -- How tall was the tower before the cycle began?
      let prevSums = fromIntegral $ sum (take (startIndex - 1) diffs)
      -- How many cycles occur, and how many steps remain after the last step?
      let (numCycles :: Integer, remainingSteps :: Integer) = (1000000000000 - (fromIntegral startIndex - 1)) `quotRem` fromIntegral cycleLength
      -- Get the cycle numbers so we can use a partial sum on the remainder
      let cycle = drop (startIndex - 1) diffs
      -- Get the remaining height additions
      let (remainingHeightAddition :: Integer) = fromIntegral $ sum $ take (fromIntegral remainingSteps) cycle
      return $ numCycles * fromIntegral sumCycle + prevSums + remainingHeightAddition


findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution _ = return Nothing

findCycle :: (MonadLogger m) => A.Array Int Int -> m (Maybe (Int, Int, Int))
findCycle arr = findCycleTail start
  where
  (start, end) = A.bounds arr

  findCycleTail :: (MonadLogger m) => Int -> m (Maybe (Int, Int, Int))
  findCycleTail startingIndex = if startingIndex >= end
    then return Nothing
    else do
      result <- tortoiseHare startingIndex (arr A.! startingIndex) (startingIndex + 1, startingIndex + 2)
      case result of
        Nothing -> findCycleTail (startingIndex + 1)
        Just r -> return $ Just r
  
  -- Are the tortoise and Hare Values equal to each other and the starting value?
  --   If so: Attempt to validate a cycle starting here.
  --   If not: Increment both and recurse
  tortoiseHare :: (MonadLogger m) => Int -> Int -> (Int, Int) -> m (Maybe (Int, Int, Int))
  tortoiseHare startingIndex startingValue (tortoise, hare)
    | hare > end = return Nothing
    | (hare - tortoise > 4) && arr A.! tortoise == startingValue && arr A.! hare == startingValue = do
      works <- tryValidate startingIndex tortoise hare tortoise
      if works
        then return $ Just (startingIndex, hare - tortoise, sumVals tortoise hare)
        else tortoiseHare startingIndex startingValue (tortoise + 1, hare + 2)
    | otherwise = tortoiseHare startingIndex startingValue (tortoise + 1, hare + 2)
  
  tryValidate :: (MonadLogger m) => Int -> Int -> Int -> Int -> m Bool
  tryValidate i1 i2 i3 goal
    | i3 > end = return False
    | i1 == goal = logErrorN ("Passed validation: " <> (pack . show $ i1) <> " " <> (pack . show $ i2) <> " " <> (pack . show $ goal)) >> return True
    | arr A.! i1 /= arr A.! i2 || arr A.! i1 /= arr A.! i3 = logErrorN ("Failed validation: " <> (pack . show $ i1) <> " " <> (pack . show $ i2)) >> return False
    | otherwise = tryValidate (i1 + 1) (i2 + 1) (i3 + 1) goal
  
  sumVals :: Int -> Int -> Int
  sumVals start end = sum [arr A.! i | i <- [start..end - 1]]


data TetrisState = TetrisState
  { currentBlock :: Block
  , occupiedSquares :: HS.HashSet Coord2
  , maxHeight :: Int
  , shapes :: [BlockType]
  , directions :: [Direction]
  , stepsRemaining :: Integer
  , stepsRun :: Integer
  , shapesCounter :: (Int, Int)
  , directionsCounter :: (Int, Int)
  , maxHeights :: A.Array Int Int
  , heightDiffs :: [Int]
  } deriving (Show)

runTetris :: (MonadFail m, MonadLogger m) => StateT TetrisState m Integer
runTetris = do
  remaining <- gets stepsRemaining
  if remaining <= 0
    then gets (fromIntegral . maxHeight)
    else do
      fallBlock
      generateNextBlock
      decrementSteps
      runTetris

pushBlock :: (MonadLogger m, MonadFail m) => StateT TetrisState m ()
pushBlock = do
  (nextDirection : restDirections) <- gets directions
  (numUsed, total) <- gets directionsCounter
  modify (\s -> s {directions = restDirections, directionsCounter = ((numUsed + 1) `mod` total, total)})
  case nextDirection of
    PushLeft -> pushLeft
    PushRight -> pushRight

pushLeft :: MonadLogger m => StateT TetrisState m ()
pushLeft = do
  block <- gets currentBlock
  occupied <- gets occupiedSquares
  when (not (hitsWall block) && leftSquaresEmpty block occupied) $ do
    let newBlock = block { leftPosition = leftPosition block - 1}
    modify (\s -> s {currentBlock = newBlock})
  where
    hitsWall (Block _ leftPos _) = leftPos <= 0
    leftSquaresToCheck (Block shape leftPos lowPos) = case shape of
      Minus -> [(leftPos - 1, lowPos)]
      Plus -> [(leftPos - 1, lowPos + 1), (leftPos, lowPos), (leftPos, lowPos + 2)]
      Ell -> [(leftPos - 1, lowPos), (leftPos + 1, lowPos + 1), (leftPos + 1, lowPos + 2)]
      Long -> [(leftPos - 1, lowPos), (leftPos - 1, lowPos + 1), (leftPos - 1, lowPos + 2), (leftPos - 1, lowPos + 3)]
      Square -> [(leftPos - 1, lowPos), (leftPos - 1, lowPos + 1)]
    leftSquaresEmpty block occupied = not $ any (`HS.member` occupied) (leftSquaresToCheck block)

pushRight :: MonadLogger m => StateT TetrisState m ()
pushRight = do
  block <- gets currentBlock
  occupied <- gets occupiedSquares
  when (not (hitsWall block) && rightSquaresEmpty block occupied) $ do
    let newBlock = block { leftPosition = leftPosition block + 1}
    modify (\s -> s {currentBlock = newBlock})
  where
    hitsWall (Block shape leftPos _) = case shape of
      Minus -> leftPos >= 3
      Plus -> leftPos >= 4
      Ell -> leftPos >= 4
      Long -> leftPos >= 6
      Square -> leftPos >= 5
    rightSquaresToCheck (Block shape leftPos lowPos) = case shape of
      Minus -> [(leftPos + 4, lowPos)]
      Plus -> [(leftPos + 3, lowPos + 1), (leftPos + 2, lowPos), (leftPos + 2, lowPos + 2)]
      Ell -> [(leftPos + 3, lowPos), (leftPos + 3, lowPos + 1), (leftPos + 3, lowPos + 2)]
      Long -> [(leftPos + 1, lowPos), (leftPos + 1, lowPos + 1), (leftPos + 1, lowPos + 2), (leftPos + 1, lowPos + 3)]
      Square -> [(leftPos + 2, lowPos), (leftPos + 2, lowPos + 1)]
    rightSquaresEmpty block occupied = not $ any (`HS.member` occupied) (rightSquaresToCheck block)

generateNextBlock :: (MonadLogger m, MonadFail m) => StateT TetrisState m ()
generateNextBlock = do
  (nextShape : restShapes) <- gets shapes
  (numShapes, total) <- gets shapesCounter
  currentMaxHeight <- gets maxHeight
  let newBlock = Block nextShape 2 (currentMaxHeight + 4)
  modify (\s -> s {shapes = restShapes, currentBlock = newBlock, shapesCounter = ((numShapes + 1) `mod` total, total)})

decrementSteps :: MonadLogger m => StateT TetrisState m ()
decrementSteps = modify (\s -> s {stepsRemaining = stepsRemaining s - 1, stepsRun = stepsRun s + 1})

initialTetrisState :: [Direction] -> Integer -> TetrisState
initialTetrisState directions totalSteps = TetrisState
  firstBlock HS.empty 0 restShapes (cycle directions) totalSteps 0 (0, 5) (0, length directions) (A.listArray (0, 6) (repeat 0)) []
  where
    (firstShape : restShapes) = cycle [Minus, Plus, Ell, Long, Square]
    firstBlock = Block firstShape 2 4

fallBlock :: (MonadLogger m, MonadFail m) => StateT TetrisState m ()
fallBlock = do
  pushBlock
  canDrop <- canDropBlock
  if canDrop
    then do
      block <- gets currentBlock
      modify (\s -> s { currentBlock = block {lowestPosition = lowestPosition block - 1} })
      fallBlock
    else settleBlock

canDropBlock :: MonadLogger m => StateT TetrisState m Bool
canDropBlock = do
  block <- gets currentBlock
  occupied <- gets occupiedSquares
  return $ downSquaresClear block occupied && lowestPosition block > 1
  where
    downSquares (Block shape leftPos lowPos) = case shape of
      Minus -> [(leftPos, lowPos - 1), (leftPos + 1, lowPos - 1), (leftPos + 2, lowPos - 1), (leftPos + 3, lowPos - 1)]
      Plus -> [(leftPos, lowPos), (leftPos + 1, lowPos - 1), (leftPos + 2, lowPos)]
      Ell -> [(leftPos, lowPos - 1), (leftPos + 1, lowPos - 1), (leftPos + 2, lowPos - 1)]
      Long -> [(leftPos, lowPos - 1)]
      Square -> [(leftPos, lowPos - 1), (leftPos + 1, lowPos - 1)]
    downSquaresClear block occupied = not $ any (`HS.member` occupied) (downSquares block)

settleBlock :: MonadLogger m => StateT TetrisState m ()
settleBlock = do
  block <- gets currentBlock
  -- logErrorN ("Settling: " <> (pack . show $ block))
  occupied <- gets occupiedSquares
  newSquares <- gets (squaresForBlock . currentBlock)
  let newOccupied = foldl (flip HS.insert) occupied newSquares
  currentMax <- gets maxHeight
  let newMax = maximum (currentMax : (snd <$> newSquares))
  currentMaxHeights <- gets maxHeights
  let updates = mapMaybe (newHeight currentMaxHeights) (upSquaresForBlock block)
  let newMaxHeights = currentMaxHeights A.// updates
  prevDiffs <- gets heightDiffs
  modify (\s -> s { maxHeight = newMax, occupiedSquares = newOccupied, maxHeights = newMaxHeights, heightDiffs = newMax - currentMax : prevDiffs})
  where
    squaresForBlock (Block shape leftPos lowPos) = case shape of
      Minus -> [(leftPos, lowPos), (leftPos + 1, lowPos), (leftPos + 2, lowPos), (leftPos + 3, lowPos)]
      Plus -> [(leftPos, lowPos + 1), (leftPos + 1, lowPos), (leftPos + 1, lowPos + 1), (leftPos + 1, lowPos + 2), (leftPos + 2, lowPos + 1)]
      Ell -> [(leftPos, lowPos), (leftPos + 1, lowPos), (leftPos + 2, lowPos), (leftPos + 2, lowPos + 1), (leftPos + 2, lowPos + 2)]
      Long -> [(leftPos, lowPos), (leftPos, lowPos + 1), (leftPos, lowPos + 2), (leftPos, lowPos + 3)]
      Square -> [(leftPos, lowPos), (leftPos, lowPos + 1), (leftPos + 1, lowPos), (leftPos + 1, lowPos + 1)]
    
    upSquaresForBlock (Block shape leftPos lowPos) = case shape of
      Minus -> [(leftPos, lowPos), (leftPos + 1, lowPos), (leftPos + 2, lowPos), (leftPos + 3, lowPos)]
      Plus -> [(leftPos, lowPos + 1), (leftPos + 1, lowPos + 2), (leftPos + 2, lowPos + 1)]
      Ell -> [(leftPos, lowPos), (leftPos + 1, lowPos), (leftPos + 2, lowPos + 2)]
      Long -> [(leftPos, lowPos + 3)]
      Square -> [(leftPos, lowPos + 1), (leftPos + 1, lowPos + 1)]
    
    newHeight currentMaxHeights (column, row) = if currentMaxHeights A.! column < row then Just (column, row) else Nothing

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