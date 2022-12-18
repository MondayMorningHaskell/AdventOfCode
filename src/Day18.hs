{-# LANGUAGE OverloadedStrings #-}

module Day18 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1)
import Text.Megaparsec.Char (eol, char)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, parsePositiveNumber, countWhere)
import qualified Data.HashSet as HS
import Control.Monad (foldM)
import qualified Data.Sequence as Seq
import Data.List (partition)

dayNum :: Int
dayNum = 18

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
type InputType = [Coord3]
type Coord3 = (Int, Int, Int)

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = sepEndBy1 parseLine eol

parseLine :: (MonadLogger m) => ParsecT Void Text m Coord3
parseLine = do
  i <- parsePositiveNumber
  char ','
  j <- parsePositiveNumber
  char ','
  k <- parsePositiveNumber
  return (i, j, k)

-------------------- SOLVING EASY --------------------
processInputEasy :: (MonadLogger m) => InputType -> m Int
processInputEasy inputs = fst <$> foldM foldLine initialFoldV inputs

type FoldType = (Int, HS.HashSet Coord3)

initialFoldV :: FoldType
initialFoldV = (0, HS.empty)

foldLine :: (MonadLogger m) => FoldType -> Coord3 -> m FoldType
foldLine (prevCount, prevSet) c@(x, y, z) = return (prevCount + newCount, HS.insert c prevSet)
  where
    newCount = 6 - 2 * countWhere (`HS.member` prevSet) neighbors
    neighbors = neighbors3 c

neighbors3 :: Coord3 -> [Coord3]
neighbors3 (x, y, z) =
  [ (x + 1, y, z)
  , (x - 1, y, z)
  , (x, y + 1, z)
  , (x, y - 1, z)
  , (x, y, z + 1)
  , (x, y, z - 1)
  ]

-------------------- SOLVING HARD --------------------
data Dimens = Dimens
  { minX :: Int
  , maxX :: Int
  , minY :: Int
  , maxY :: Int
  , minZ :: Int
  , maxZ :: Int
  } deriving (Show)

processInputHard :: (MonadLogger m) => InputType -> m Int
processInputHard inputs = do
  let cubeSet = HS.fromList inputs
      (xs, ys, zs) = unzip3 inputs
      dimens = Dimens (minimum xs - 1) (maximum xs + 1) (minimum ys - 1) (maximum ys + 1) (minimum zs - 1) (maximum zs + 1)
      initialLoc = (minX dimens, minY dimens, minZ dimens)
  bfs dimens cubeSet (0, Seq.singleton initialLoc, HS.singleton initialLoc)

bfs :: (MonadLogger m) => Dimens -> HS.HashSet Coord3 -> (Int, Seq.Seq Coord3, HS.HashSet Coord3) -> m Int
bfs dimens cubeSet (count, queue, visited) = case Seq.viewl queue of
  Seq.EmptyL -> return count
  top Seq.:< rest -> do
    let neighbors = filter (\c -> inBounds dimens c && not (HS.member c visited)) (neighbors3 top)
        (inLava, notLava) = partition (`HS.member` cubeSet) neighbors
        newQueue = foldl (Seq.|>) rest notLava
        newVisited = foldl (flip HS.insert) visited notLava
    bfs dimens cubeSet (count + length inLava, newQueue, newVisited)

inBounds :: Dimens -> Coord3 -> Bool
inBounds (Dimens mnx mxx mny mxy mnz mxz) (x, y, z)  =
  x >= mnx && x <= mxx && y >= mny && y <= mxy && z >= mnz && z <= mxz

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