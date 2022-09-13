{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Control.Monad (forM_, when)
import Control.Monad.Logger
import qualified Data.Map as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (pack)
import qualified Data.Text as T
import Text.Megaparsec (runParser, sepBy, some, MonadParsec (eof), someTill)
import Utils (parseCSVInts, parse2DMapSpace, OccMap, incKeyWithOcc)
import Text.Megaparsec.Char (eol)
import Data.Tuple (swap)
import Data.List (find, sort, partition)
import Data.List.Extra (groupOn)

d4ES :: IO (Maybe Int)
d4ES = solveDay4Easy "inputs/day_4_small.txt"

d4EB :: IO (Maybe Int)
d4EB = solveDay4Easy "inputs/day_4_big.txt"

d4HS :: IO (Maybe Int)
d4HS = solveDay4Hard "inputs/day_4_small.txt"

d4HB :: IO (Maybe Int)
d4HB = solveDay4Hard "inputs/day_4_big.txt"

solveDay4Easy :: FilePath -> IO (Maybe Int)
solveDay4Easy fp = do
    (numbers, boardMaps) <- readFile fp >>= parseInputs
    runStdoutLoggingT $ do
      boards <- mapM createBoard boardMaps
      logDebugN (pack . show $ length boards)
      playBoards numbers boards

playBoards :: (MonadLogger m) => [Int] -> [Board] -> m (Maybe Int)
playBoards [] _ = return Nothing
playBoards (i : rest) boards = do
  newBoards <- mapM (processNumber i) boards
  case find hasWon newBoards of
    Just b -> Just <$> scoreForWinningBoard b i
    Nothing -> playBoards rest newBoards

playBoardsHard :: (MonadLogger m) => [Int] -> [Board] -> m (Maybe Int)
playBoardsHard [] _ = return Nothing
playBoardsHard _ [] = return Nothing
playBoardsHard (i : rest) boards = do
  newBoards <- mapM (processNumber i) boards
  when (null newBoards) $ logErrorN "Processed number and found no boards!"
  let (winningBoards, nonWinningBoards) = partition hasWon newBoards
  if null nonWinningBoards
    then Just <$> scoreForWinningBoard (head winningBoards) i
    else playBoardsHard rest nonWinningBoards

solveDay4Hard :: FilePath -> IO (Maybe Int)
solveDay4Hard fp = do
    (numbers, boardMaps) <- readFile fp >>= parseInputs
    runStdoutLoggingT $ do
      boards <- mapM createBoard boardMaps
      logDebugN (pack . show $ length boards)
      playBoardsHard numbers boards

parseInputs :: String -> IO ([Int], [HashMap (Int, Int) Int])
parseInputs input = case runParser myParser "Day4.hs" (pack input) of
  Left e -> error $ "Failed to parse day 4: " ++ show e
  Right x -> return x
  where
    myParser = do
      numbersToCall <- parseCSVInts <* eol
      maps <- someTill (parse2DMapSpace 5) eof
      return (numbersToCall, maps)

data Board = Board
  { rows :: OccMap Int
  , cols :: OccMap Int
  , numbers :: HashMap Int (Int, Int)
  , unmarked :: HashSet (Int, Int)
  , numRows :: Word
  , numCols :: Word
  , hasWon :: Bool
  }

-- Assume value numbers are unique
-- This function makes two assumptions about the input HashMap
-- 1. The keys of the map cover a continuous 2x2 range, starting from (0,0) up to (maxRow-1,maxCol-1)
-- 2. The numbers within the board are unique
-- TODO: The first condition could be more accurately captured by using an array
--       The second condition should be checked explicitly and this function should return Maybe
--       OR it could be handled by using HashMap Int [(Int, Int)] in the Board type.
createBoard :: (MonadLogger m) => HashMap (Int, Int) Int -> m Board
createBoard input = do
  return $ Board M.empty M.empty (HM.fromList $ swap <$> pairs) (HS.fromList $ map fst pairs)
    (fromIntegral $ maxRow + 1) (fromIntegral maxCol + 1) False
  where
    pairs = HM.toList input
    allCoordinates = fst <$> pairs
    (maxRow, maxCol) = maximum allCoordinates

processNumber :: (MonadLogger m) => Int -> Board -> m Board
processNumber i board = case HM.lookup i (numbers board) of
  Nothing -> return board
  Just (r, c) -> do
    let (newRows, numInRow) = incKeyWithOcc (rows board) r
        (newCols, numInCol) = incKeyWithOcc (cols board) c
        newUnmarked = HS.delete (r, c) (unmarked board)
        isWinningMove = numInRow >= numRows board || numInCol >= numCols board
    return board {rows = newRows, cols = newCols, hasWon = isWinningMove, unmarked = newUnmarked}

scoreForWinningBoard :: (MonadLogger m) => Board -> Int -> m Int
scoreForWinningBoard board finalNumber = do
  logBoard board
  return $ finalNumber * sumUnmarked
  where
    sumUnmarked = sum $ map fst $ filter (\(i, coord) -> HS.member coord (unmarked board)) $ HM.toList (numbers board)

logBoard :: (MonadLogger m) => Board -> m ()
logBoard b = do
  -- forM_ rows $ \row -> logDebugN $
    logDebugN $ pack . show $ numberSet
  where
    numberSet :: [((Int, Int), Int)]
    numberSet = sort $ swap <$> HM.toList (numbers b)
    -- rows :: [[Int]]
    -- rows = map (map snd) (groupOn (fst . fst) numberSet)

  {-
  { rows :: OccMap Int
  , cols :: OccMap Int
  , numbers :: HashMap Int (Int, Int)
  , unmarked :: HashSet (Int, Int)
  , numRows :: Word
  , numCols :: Word
  , hasWon :: Bool
  -}