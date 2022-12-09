{-# LANGUAGE OverloadedStrings #-}

module Day9 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1, (<|>))
import Text.Megaparsec.Char (eol, char)
import qualified Data.Set as S
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, parsePositiveNumber, Coord2)
import Control.Monad (foldM)

dayNum :: Int
dayNum = 9

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO (Maybe Int)
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  logErrorN (pack . show $ input)
  Just <$> processInputEasy input

solveHard :: FilePath -> IO (Maybe Int)
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  Just <$> processInputHard input

-------------------- PARSING --------------------
data Move = UpMove | RightMove | DownMove | LeftMove
  deriving (Show)

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = concat <$> sepEndBy1 parseLine eol

type InputType = [Move]
type LineType = [Move]

parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
parseLine = do
  move <- up <|> right <|> down <|> left
  char ' '
  i <- parsePositiveNumber
  return $ replicate i move
  where
    up = char 'U' >> return UpMove
    right = char 'R' >> return RightMove
    down = char 'D' >> return DownMove
    left = char 'L' >> return LeftMove

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy inputs = do
  (finalSet, _) <- foldM foldMove (initialFoldV 2) inputs
  return $ S.size finalSet

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard inputs = do
  (finalSet, _) <- foldM foldMove (initialFoldV 10) inputs
  return $ S.size finalSet

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution _ = return Nothing

type FoldType = (S.Set Coord2, [Coord2])

initialFoldV :: Int -> FoldType
initialFoldV n = (S.empty, replicate n (0, 0))

foldMove :: (MonadLogger m) => FoldType -> Move -> m FoldType
foldMove (prevSet, knots) move = if null knots then logErrorN "Invalid case, empty knots list!" >> return (prevSet, knots)
  else do
    newLocations <- hardFoldTail [nextHead (head knots) move] (tail knots)
    return (S.insert (head newLocations) prevSet, reverse newLocations)
  where
    hardFoldTail :: (MonadLogger m) => [Coord2] -> [Coord2] -> m [Coord2]
    hardFoldTail [] _ = logErrorN "Invalid case!" >> return []
    hardFoldTail done [] = return done
    hardFoldTail done@(head : _) (next : rest) = hardFoldTail (nextTail head next : done) rest

-------------------- SOLUTION PATTERNS --------------------

nextHead :: Coord2 -> Move -> Coord2
nextHead (headX, headY) move = case move of
  UpMove -> (headX + 1, headY)
  RightMove -> (headX, headY + 1)
  DownMove -> (headX - 1, headY)
  LeftMove -> (headX, headY - 1)

nextTail :: Coord2 -> Coord2 -> Coord2
nextTail head@(headX, headY) tail@(tailX, tailY) 
  | dontMove = tail
  | headX == tailX = (tailX, if tailY < headY then tailY + 1 else tailY - 1)
  | headY == tailY = (if tailX > headX then tailX - 1 else tailX + 1, tailY)
  | q1 = (tailX + 1, tailY + 1)
  | q2 = (tailX + 1, tailY - 1)
  | q3 = (tailX - 1, tailY - 1)
  | otherwise = (tailX - 1, tailY + 1)
  where
    dontMove = abs (headX - tailX) <= 1 && abs (headY - tailY) <= 1
    q1 = headX > tailX && headY > tailY
    q2 = headX > tailX && headY < tailY
    q3 = headX < tailX && headY < tailY

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