{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day2 where

import Control.Monad (foldM)
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Text.Megaparsec (ParsecT, sepEndBy1, some, sepEndBy1, (<|>), try, optional, sepBy1)
import Text.Megaparsec.Char (eol, digitChar, string, letterChar, char)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile)

dayNum :: Int
dayNum = 2

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO Int
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  processInputEasy input

solveHard :: FilePath -> IO Int
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  processInputHard input

-------------------- PARSING --------------------
type InputType = [Game]

-- Each 3-tuple is blue->green->red
type Game = (Int, [(Int, Int, Int)])

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput =
  sepEndBy1 parseLine eol

parseLine :: (MonadLogger m) => ParsecT Void Text m Game
parseLine = do
  string "Game "
  gameId <- read <$> some digitChar
  string ": "
  pulls <- sepBy1 parseSinglePull (string "; ") 
  return $ (gameId, pulls)
  where
    parseSinglePull = do
      numColors <- sepBy1 parseNumColor (string ", ")
      foldM f (0, 0, 0) numColors
    
    f (pb, pg, pr) (num, color) = case color of
        "blue" -> return (num, pg, pr)
        "green" -> return (pb, num, pr)
        "red" -> return (pb, pg, num)
        x -> fail $ "Invalid color: " <> x <> "!"
    
    parseNumColor = do
      num <- read <$> some digitChar
      char ' '
      color <- some letterChar
      return (num, color)

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy games = do
  return $ sum (fst <$> possibleGames)
  where
    possibleGames = filter isPossible games

    isPossible :: Game -> Bool
    isPossible (_, pulls) = null
      (filter (\(b, g, r) -> b > 14 || g > 13 || r > 12)
      pulls)

findEasySolution :: (MonadLogger m) => EasySolutionType -> m Int
findEasySolution _ = return 0

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard games = return $ sum (gamePower <$> games)
  where
    gamePower :: Game -> Int
    gamePower (_, pulls) =
      let (minBlue, minGreen, minRed) = foldl g (0, 0, 0) pulls
      in  minBlue * minGreen * minRed
    
    g :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
    g (nb, ng, nr) (minB, minG, minR) = (max nb minB, max ng minG, max nr minR)

findHardSolution :: (MonadLogger m) => HardSolutionType -> m Int
findHardSolution _ = return 0

-------------------- SOLUTION PATTERNS --------------------

-- solveFold :: (MonadLogger m) => [LineType] -> m EasySolutionType
-- solveFold = foldM foldLine initialFoldV

-- type FoldType = ()

-- initialFoldV :: FoldType
-- initialFoldV = undefined

-- foldLine :: (MonadLogger m) => FoldType -> LineType -> m FoldType
-- foldLine = undefined

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

easySmall :: IO Int
easySmall = solveEasy smallFile

easyLarge :: IO Int
easyLarge = solveEasy largeFile

hardSmall :: IO Int
hardSmall = solveHard smallFile

hardLarge :: IO Int
hardLarge = solveHard largeFile