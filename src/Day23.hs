{-# LANGUAGE OverloadedStrings #-}

module Day23 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1, (<|>), some)
import Text.Megaparsec.Char (eol, char)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, parse2DHashMap, Coord2, OccMap, emptyOcc, addKey, incKey)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map
import Data.Maybe (isJust, fromJust)
import Control.Monad (foldM, forM_)
import Control.Monad.Cont (MonadIO (liftIO))

dayNum :: Int
dayNum = 23

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO (Maybe Int)
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  result <- processInputEasy input
  findEasySolution result

solveHard :: FilePath -> IO (Maybe Int)
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  Just <$> processInputHard input

-------------------- PARSING --------------------
data Direction = North | South | East | West
  deriving (Show, Eq)

type InputType = HS.HashSet Coord2

-- Parse as 2D Hash Map of Bools.
-- Filter out to the coordinates that are occupied.
parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = do
  hashMap <- parse2DHashMap (some parseLoc)
  return $ HS.fromList $ fst <$> filter snd (HM.toList hashMap)
  where
    parseLoc = (char '.' >> return False) <|> (char '#' >> return True)

-------------------- SOLVING EASY --------------------
type EasySolutionType = HS.HashSet Coord2

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy inputs = do
  (result, _, _) <- solveStateN 10 (inputs, [North, South, West, East], True)
  return result

{-
1. The set of coordinates occupied by elves
2. The current direction priority (rotates each round)
3. Whether or not any elf moved this round.
-}
type StateType = (HS.HashSet Coord2, [Direction], Bool)

-- Recursively run the state evolution n times.
solveStateN :: (MonadLogger m) => Int -> StateType -> m StateType
solveStateN 0 st = return st {- Base case: (n = 0) -}
solveStateN n st = do
  st' <- evolveState st
  solveStateN (n - 1) st' {- Recursive case: (n - 1) -}

{-
  1. Get all propsoed moves from the elves.
      a. Using a fold, track the number of times each proposed coordinate appears.
      b. Also track a mapping from the proposed square back to the source.
      c. Check if each direction is empty (and stand still if all of them are)
      d. Try a move in each direction, in the priority given by the priority list.
      e. If we get a successful move, update the occurrence map and occupied set.
  2. Exclude proposed moves with more than 1 elf moving there.
  3. Update the set of occupied squares
      a. Look up the original space in the map.
      b. Delete the original space from our occupied map (we don't have to worrry about another elf moving here)
      c. Insert the new space.
  4. Update the state by rotating the directions and determining if any moved.
-}
evolveState :: (MonadLogger m) => StateType -> m StateType
evolveState (elfSet, directions, _) = do
  {-1-}
  (proposedMoves, occurrences) <- foldM proposeMove (HM.empty, emptyOcc) elfSet
  {-2-}
  let spacesWithOne = filter (\(_, occ) -> occ == 1) (Data.Map.toList occurrences)
  {-3-}
  let updatedSet = foldl (updateSetForMove proposedMoves) elfSet (fst <$> spacesWithOne)
  return (updatedSet, rotatedDirections, not (null spacesWithOne))
  where
    {-4-}
    rotatedDirections = tail directions ++ [head directions]

    {-1-}
    proposeMove :: (MonadLogger m) => (HM.HashMap Coord2 Coord2, OccMap Coord2) -> Coord2 -> m (HM.HashMap Coord2 Coord2, OccMap Coord2)
    proposeMove (prevMoves, destOcc) c@(row, col) = do
{-1c-}let northEmpty = not $ or [HS.member c elfSet | c <- [(row - 1, col - 1), (row - 1, col), (row - 1, col + 1)]]
{-1c-}    southEmpty = not $ or [HS.member c elfSet | c <- [(row + 1, col - 1), (row + 1, col), (row + 1, col + 1)]]
{-1c-}    westEmpty = not $ or [HS.member c elfSet | c <- [(row + 1, col - 1), (row , col - 1), (row - 1, col - 1)]]
{-1c-}    eastEmpty = not $ or [HS.member c elfSet | c <- [(row + 1, col + 1), (row , col + 1), (row - 1, col + 1)]]
{-1c-}    stayStill = northEmpty && southEmpty && eastEmpty && westEmpty
{-1d-}    trialMove = case head directions of
{-1d-}                  North -> tryNorth northEmpty c <|> trySouth southEmpty c <|> tryWest westEmpty c <|> tryEast eastEmpty c
{-1d-}                  South -> trySouth southEmpty c <|> tryWest westEmpty c <|> tryEast eastEmpty c <|> tryNorth northEmpty c
{-1d-}                  West -> tryWest westEmpty c <|> tryEast eastEmpty c <|> tryNorth northEmpty c <|> trySouth southEmpty c
{-1d-}                  East -> tryEast eastEmpty c <|> tryNorth northEmpty c <|> trySouth southEmpty c <|> tryWest westEmpty c
      return $ if isJust trialMove && not stayStill 
      {-1e-}then (HM.insert (fromJust trialMove) c prevMoves, incKey destOcc (fromJust trialMove))
            else (prevMoves, destOcc)

    {-3-}
    updateSetForMove :: HM.HashMap Coord2 Coord2 -> HS.HashSet Coord2 -> Coord2 -> HS.HashSet Coord2
    updateSetForMove moveLookup prevSet newLoc = HS.insert newLoc (HS.delete (moveLookup HM.! newLoc) prevSet)

{-1d-}
tryNorth :: Bool -> Coord2 -> Maybe Coord2
tryNorth b (row, col) = if b then Just (row - 1, col) else Nothing

trySouth :: Bool -> Coord2 -> Maybe Coord2
trySouth b (row, col) = if b then Just (row + 1, col) else Nothing

tryEast :: Bool -> Coord2 -> Maybe Coord2
tryEast b (row, col) = if b then Just (row, col + 1) else Nothing

tryWest :: Bool -> Coord2 -> Maybe Coord2
tryWest b (row, col) = if b then Just (row, col - 1) else Nothing


findEasySolution :: (MonadLogger m, MonadIO m) => EasySolutionType -> m (Maybe Int)
findEasySolution occupiedSquares = do
  let (rows, cols) = unzip $ HS.toList occupiedSquares
  let r@(minRow, maxRow, minCol, maxCol) = (minimum rows, maximum rows, minimum cols, maximum cols)
  return $ Just $ (maxRow - minRow + 1) * (maxCol - minCol + 1) - HS.size occupiedSquares

-------------------- SOLVING HARD --------------------
type HardSolutionType = Int

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard inputs = solveStateEnd 0 (inputs, [North, South, West, East], True)

-- Evolve the state until no more elves move.
solveStateEnd :: (MonadLogger m) => Int -> StateType -> m Int
solveStateEnd n st@(_, _, False) = return n {- Base Case: No elves moved. -}
solveStateEnd n st = do
  st' <- evolveState st
  solveStateEnd (n + 1) st' {- Recursive Case: Add 1 to count -}

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution _ = return Nothing

-------------------- SOLUTION PATTERNS --------------------


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