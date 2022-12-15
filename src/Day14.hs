{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day14 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1, sepBy1)
import Text.Megaparsec.Char (eol, string, char)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, Coord2, parsePositiveNumber)
import qualified Data.HashSet as HS
import Data.List (sort)
import Control.Monad (foldM)
import Control.Monad.Cont (lift)

dayNum :: Int
dayNum = 14

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
type InputType = HS.HashSet Coord2

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = do
  coordLines <- sepEndBy1 parseLine eol
  lift $ buildInitialMap coordLines

buildInitialMap :: (MonadLogger m) => [[Coord2]] -> m (HS.HashSet Coord2)
buildInitialMap = foldM f HS.empty
  where
    f :: (MonadLogger m) => HS.HashSet Coord2 -> [Coord2] -> m (HS.HashSet Coord2)
    f prevSet [] = return prevSet
    f prevSet [_] = return prevSet
    f prevSet (firstCoord : secondCoord : rest) = do
      newCoords <- makeLine firstCoord secondCoord
      f (foldl (flip HS.insert) prevSet newCoords) (secondCoord : rest)

    makeLine :: (MonadLogger m) => Coord2 -> Coord2 -> m [Coord2]
    makeLine a@(a1, a2) b@(b1, b2) 
      | a1 == b1 = return $ map (a1,) (if a2 >= b2 then [b2,(b2+1)..a2] else [a2,(a2+1)..b2])
      | a2 == b2 = return $ map (,b2) (if a1 >= b1 then [b1,(b1+1)..a1] else [a1,(a1+1)..b1])
      | otherwise = logErrorN ("Line is neither horizontal nor vertical: " <> (pack . show $ (a, b))) >> return []

-- parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
-- parseInput =

-- type InputType = [LineType]
-- type LineType = ()

parseLine :: Monad m => ParsecT Void Text m [Coord2]
parseLine = sepBy1 parseNumbers (string " -> ")
  where
    parseNumbers = do
      i <- parsePositiveNumber 
      char ','
      j <- parsePositiveNumber
      return (i, j)

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy inputWalls = do
  let maxY = maximum $ snd <$> HS.toList inputWalls
  evolveState maxY (inputWalls, 0)

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard inputWalls = do
  let maxY = maximum $ snd <$> HS.toList inputWalls
  evolveState' maxY (inputWalls, 0)

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution _ = return Nothing

-------------------- SOLUTION PATTERNS --------------------

-- solveFold :: (MonadLogger m) => [LineType] -> m EasySolutionType
-- solveFold = foldM foldLine initialFoldV

-- type FoldType = ()

-- initialFoldV :: FoldType
-- initialFoldV = undefined

-- foldLine :: (MonadLogger m) => FoldType -> LineType -> m FoldType
-- foldLine = undefined

evolveState :: (MonadLogger m) => Int -> (HS.HashSet Coord2, Int) -> m Int
evolveState maxY (filledSpaces, prevSands) = do
  (newSet, landed) <- dropSand maxY (500, 0) filledSpaces
  if landed
    then evolveState maxY (newSet, prevSands + 1)
    else return prevSands

dropSand :: (MonadLogger m) => Int -> Coord2 -> HS.HashSet Coord2 -> m (HS.HashSet Coord2, Bool)
dropSand maxY (x, y) filledSpaces
  | y > maxY = return (filledSpaces, False)
  | not (HS.member (x, y + 1) filledSpaces) = dropSand maxY (x, y + 1) filledSpaces
  | not (HS.member (x - 1, y + 1) filledSpaces) = dropSand maxY (x - 1, y + 1) filledSpaces
  | not (HS.member (x + 1, y + 1) filledSpaces) = dropSand maxY (x + 1, y + 1) filledSpaces
  | otherwise = return (HS.insert (x, y) filledSpaces, True)

evolveState' :: (MonadLogger m) => Int -> (HS.HashSet Coord2, Int) -> m Int
evolveState' maxY (filledSpaces, prevSands) = do
  (newSet, landed) <- dropSand' maxY (500, 0) filledSpaces
  if landed
    then evolveState' maxY (newSet, prevSands + 1)
    else return (prevSands + 1)

dropSand' :: (MonadLogger m) => Int -> Coord2 -> HS.HashSet Coord2 -> m (HS.HashSet Coord2, Bool)
dropSand' maxY (x, y) filledSpaces
  | y > maxY = return (HS.insert (x, y) filledSpaces, True)
  | not (HS.member (x, y + 1) filledSpaces) = dropSand' maxY (x, y + 1) filledSpaces
  | not (HS.member (x - 1, y + 1) filledSpaces) = dropSand' maxY (x - 1, y + 1) filledSpaces
  | not (HS.member (x + 1, y + 1) filledSpaces) = dropSand' maxY (x + 1, y + 1) filledSpaces
  | otherwise = return (HS.insert (x, y) filledSpaces, (x, y) /= (500, 0))

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