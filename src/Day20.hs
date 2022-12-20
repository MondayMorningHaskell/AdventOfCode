{-# LANGUAGE OverloadedStrings #-}

module Day20 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN, logDebugN)
import Text.Megaparsec (ParsecT, sepEndBy1)
import Text.Megaparsec.Char (eol)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, parseSignedInteger)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Internal as Seq
import Data.List (partition)
import Data.Int (Int64)

dayNum :: Int
dayNum = 20

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO (Maybe Int)
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  Just <$> processInputEasy input

solveHard :: FilePath -> IO Int64
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  processInputHard input

-------------------- PARSING --------------------
type InputType = [Int]

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = sepEndBy1 parseSignedInteger eol

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy inputs = do
  finalSeq <- easyTail (initialFoldV inputs)
  let first0 = Seq.findIndexL (== 0) finalSeq
  case first0 of
    Nothing -> logErrorN "Couldn't find 0!" >> return minBound
    Just i -> do
      let indices = map (`mod` Seq.length finalSeq) [i + 1000, i + 2000, i + 3000]
      return $ sum $ map (Seq.index finalSeq) indices

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = Int64

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard inputs = do
  let first = initialFoldH inputs
  set1 <- hardTail (initialFoldH inputs)
  logErrorN "Done 1"
  let nextIndices1 = newIndices set1
  set2 <- hardTail (set1, nextIndices1)
  logErrorN "Done 2"
  let nextIndices2 = newIndices set2
  set3 <- hardTail (set2, nextIndices2)
  logErrorN "Done 3"
  let nextIndices3 = newIndices set3
  set4 <- hardTail (set3, nextIndices3)
  logErrorN "Done 4"
  let nextIndices4 = newIndices set4
  set5 <- hardTail (set4, nextIndices4)
  logErrorN "Done 5"
  let nextIndices5 = newIndices set5
  set6 <- hardTail (set5, nextIndices5)
  logErrorN "Done 6"
  let nextIndices6 = newIndices set6
  set7 <- hardTail (set6, nextIndices6)
  logErrorN "Done 7"
  let nextIndices7 = newIndices set7
  set8 <- hardTail (set7, nextIndices7)
  logErrorN "Done 8"
  let nextIndices8 = newIndices set8
  set9 <- hardTail (set8, nextIndices8)
  logErrorN "Done 9"
  let nextIndices9 = newIndices set9
  set10 <- hardTail (set9, nextIndices9)
  logErrorN "Done 10"
  let first0 = Seq.findIndexL (\(v, _) -> v == 0) set10
  case first0 of
    Nothing -> logErrorN "Couldn't find 0!" >> return minBound
    Just i -> do
      let indices = map (`mod` Seq.length set10) [i + 1000, i + 2000, i + 3000]
      return $ sum $ map (fst . Seq.index set10) indices

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution _ = return Nothing

-------------------- SOLUTION PATTERNS --------------------

type FoldType = (Seq.Seq Int, [Int])

initialFoldV :: [Int] -> FoldType
initialFoldV inputs = (Seq.fromList inputs, [0,1..(length inputs - 1)])

easyTail :: (MonadLogger m) => FoldType -> m (Seq.Seq Int)
easyTail (queue, []) = return queue
easyTail (queue, nextIndex : restIndices) = do
  let val = Seq.index queue nextIndex
  let queue' = Seq.deleteAt nextIndex queue
  let newIndex = (nextIndex + val) `mod` Seq.length queue'
  let queue'' = Seq.insertAt newIndex val queue'
  let (indicesToChange, unchanged) = partition (<= newIndex) restIndices
  easyTail (queue'', map (\i -> i - 1) indicesToChange ++ unchanged)

type FoldTypeH = (Seq.Seq (Int64, Int), [Int])

initialFoldH :: [Int] -> FoldTypeH
initialFoldH inputs = (Seq.fromList tuples, [0,1..(length inputs - 1)])
  where
    indices = [0,1..(length inputs - 1)]
    tuples = zip (map ((* 811589153) . fromIntegral) inputs) indices

hardTail :: (MonadLogger m) => FoldTypeH -> m (Seq.Seq (Int64, Int))
hardTail (queue, []) = return queue
hardTail (queue, nextIndex : restIndices) = do
  let (val, order) = Seq.index queue nextIndex
  let queue' = Seq.deleteAt nextIndex queue
  let val' = fromIntegral (val `mod` fromIntegral (Seq.length queue'))
  let newIndex = (nextIndex + val') `mod` Seq.length queue'
  let queue'' = Seq.insertAt newIndex (val, order) queue'
  let finalIndices = adjustIndices nextIndex newIndex
  hardTail (queue'', finalIndices)
  where
    adjustIndices old new 
      | old > new = map (\i -> if i >= new && i < old then i + 1 else i) restIndices
      | old < new = map (\i -> if i <= new && i > old then i - 1 else i) restIndices
      | otherwise = restIndices

newIndices :: Seq.Seq (Int64, Int) -> [Int]
newIndices inputs = seqToList (fst <$> sortedByOrder)
  where
    zipped = Seq.zip (Seq.fromList [0,1..(Seq.length inputs - 1)]) inputs
    sortedByOrder = Seq.sortOn (snd . snd) zipped

seqToList :: Seq.Seq a -> [a]
seqToList sequence = reverse $ foldl (flip (:)) [] sequence

-- i is the index in order
-- they will go in the order given by J
-- (i, (v, j))

-------------------- BOILERPLATE --------------------
smallFile :: FilePath
smallFile = "inputs_2022/day_" <> show dayNum <> "_small.txt"

largeFile :: FilePath
largeFile = "inputs_2022/day_" <> show dayNum <> "_large.txt"

easySmall :: IO (Maybe Int)
easySmall = solveEasy smallFile

easyLarge :: IO (Maybe Int)
easyLarge = solveEasy largeFile

hardSmall :: IO Int64
hardSmall = solveHard smallFile

hardLarge :: IO Int64
hardLarge = solveHard largeFile