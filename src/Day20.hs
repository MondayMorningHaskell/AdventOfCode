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
  finalSeq <- easyTail (initialEasy inputs)
  let first0 = Seq.findIndexL (== 0) finalSeq
  case first0 of
    Nothing -> logErrorN "Couldn't find 0!" >> return minBound
    Just i -> do
      let indices = map (`mod` Seq.length finalSeq) [i + 1000, i + 2000, i + 3000]
      return $ sum $ map (Seq.index finalSeq) indices

type EasyState = (Seq.Seq Int, [Int])

initialEasy :: [Int] -> EasyState
initialEasy inputs = (Seq.fromList inputs, [0,1..(length inputs - 1)])

easyTail :: (MonadLogger m) => EasyState -> m (Seq.Seq Int)
easyTail (queue, []) = return queue
easyTail (queue, nextIndex : restIndices) = do
  let val = Seq.index queue nextIndex
      queue' = Seq.deleteAt nextIndex queue
      newIndex = (nextIndex + val) `mod` Seq.length queue'
      queue'' = Seq.insertAt newIndex val queue'
      (indicesToChange, unchanged) = partition (<= newIndex) restIndices
  easyTail (queue'', map (\i -> i - 1) indicesToChange ++ unchanged)

-------------------- SOLVING HARD --------------------
type HardSolutionType = Int64

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard inputs = do
  finalSet <- solveN 10 (initialHard inputs)
  let first0 = Seq.findIndexL (\(v, _) -> v == 0) finalSet
  case first0 of
    Nothing -> logErrorN "Couldn't find 0!" >> return minBound
    Just i -> do
      let indices = map (`mod` Seq.length finalSet) [i + 1000, i + 2000, i + 3000]
      return $ sum $ map (fst . Seq.index finalSet) indices

type HardState = (Seq.Seq (Int64, Int), [Int])

solveN :: (MonadLogger m) => Int -> HardState -> m (Seq.Seq (Int64, Int))
solveN 0 (queue, _) = return queue
solveN n (queue, indices) = do
  newSet <- hardTail (queue, indices)
  let nextIndices = newIndices newSet
  solveN (n - 1) (newSet, nextIndices)

initialHard :: [Int] -> HardState
initialHard inputs = (Seq.fromList tuples, [0,1..(length inputs - 1)])
  where
    indices = [0,1..(length inputs - 1)]
    tuples = zip (map ((* 811589153) . fromIntegral) inputs) indices

hardTail :: (MonadLogger m) => HardState -> m (Seq.Seq (Int64, Int))
hardTail (queue, []) = return queue
hardTail (queue, nextIndex : restIndices) = do
  let (val, order) = Seq.index queue nextIndex
      queue' = Seq.deleteAt nextIndex queue
      val' = fromIntegral (val `mod` fromIntegral (Seq.length queue'))
      newIndex = (nextIndex + val') `mod` Seq.length queue'
      queue'' = Seq.insertAt newIndex (val, order) queue'
      finalIndices = adjustIndices nextIndex newIndex
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