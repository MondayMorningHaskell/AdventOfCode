{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Text.Megaparsec (ParsecT, sepEndBy1, some)
import Text.Megaparsec.Char (eol, digitChar, string, hspace1, char, hspace)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, parseSpacedInts, OccMapBig, occLookup, addKey)
import qualified Data.Set as S
import qualified Data.Map as M

dayNum :: Int
dayNum = 4

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO Int
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  processInputEasy input

solveHard :: FilePath -> IO Integer
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  processInputHard input

-------------------- PARSING --------------------

type InputType = [LineType]
type LineType = (S.Set Int, S.Set Int)

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput =
  sepEndBy1 parseLine eol

parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
parseLine = do
  string "Card"
  hspace
  some digitChar
  char ':'
  hspace
  winners <- S.fromList <$> intSpaceParser
  char '|'
  hspace
  numbers <- S.fromList <$> intSpaceParser
  return (winners, numbers)
  where
    intSpaceParser = fmap read <$> sepEndBy1 (some digitChar) hspace1

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy cards = return $ sum (scoreCard <$> cards)
  where
    scoreCard (winners, numbers) =
      let matches = S.size (S.intersection winners numbers)
      in  if matches == 0 then 0 else 2 ^ (matches - 1)

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = Integer

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard cards =
  let x = foldl f M.empty cards'
  in  return $ sum (M.elems x) + fromIntegral (length cards)
  where
    cards' = zip [1,2..] cards

    f :: OccMapBig Int -> (Int, (S.Set Int, S.Set Int)) -> OccMapBig Int
    f prevMap (idn, (winners, numbers)) =
      let matches = S.size (S.intersection winners numbers)
          copies = occLookup prevMap idn
          valsToIncrease = [idn+1,idn+2..idn+matches]
      in  foldl (\pm v -> addKey pm v (copies + 1)) prevMap valsToIncrease

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

hardSmall :: IO Integer
hardSmall = solveHard smallFile

hardLarge :: IO Integer
hardLarge = solveHard largeFile