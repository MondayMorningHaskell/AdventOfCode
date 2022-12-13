{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Data.Functor ((<&>))
import Text.Megaparsec (ParsecT, sepEndBy1, (<|>), sepBy1, sepBy)
import Text.Megaparsec.Char (eol, char)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, parsePositiveNumber)
import Control.Monad
import Data.List (sortBy, elemIndex)

dayNum :: Int
dayNum = 13

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
type InputType = [(Packet, Packet)]

data Packet =
  IntPacket Int |
  ListPacket [Packet]
  deriving (Show, Eq)

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = sepEndBy1 parsePacketPair eol

parsePacketPair :: (MonadLogger m) => ParsecT Void Text m (Packet, Packet)
parsePacketPair = do
  p1 <- parsePacket
  eol
  p2 <- parsePacket
  eol
  return (p1, p2)

parsePacket :: (MonadLogger m) => ParsecT Void Text m Packet
parsePacket = parseInt <|> parseList
  where
    parseInt = parsePositiveNumber <&> IntPacket
    parseList = do
      char '['
      packets <- sepBy parsePacket (char ',')
      char ']'
      return $ ListPacket packets

-- parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
-- parseInput =
--   sepEndBy1 parseLine eol

-- type InputType = [LineType]
-- type LineType = ()

-- parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
-- parseLine = return ()

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy inputs = foldM foldLine initialFoldV (zip [1,2..] inputs)

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard inputs = do
  let divider1 = ListPacket [ListPacket [IntPacket 2]]
  let divider2 = ListPacket [ListPacket [IntPacket 6]]
      newInputs = (divider1, divider2) : inputs
      sortedPackets = sortBy evalPackets $ concat (pairToList <$> newInputs)
      i1 = elemIndex divider1 sortedPackets
      i2 = elemIndex divider2 sortedPackets
  case (i1, i2) of
    (Just index1, Just index2) -> return $ (index1 + 1) * (index2 + 1)
    _ -> return (-1)
  where
    pairToList (a, b) = [a, b]

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution _ = return Nothing

-------------------- SOLUTION PATTERNS --------------------

-- solveFold :: (MonadLogger m) => [LineType] -> m EasySolutionType
-- solveFold = 

type FoldType = Int

initialFoldV :: FoldType
initialFoldV = 0

foldLine :: (MonadLogger m) => FoldType -> (Int, (Packet, Packet)) -> m FoldType
foldLine prev (index, (p1, p2)) = do
  let rightOrder = evalPackets p1 p2
  return $ if rightOrder == LT then prev + index else prev


evalPackets :: Packet -> Packet -> Ordering
evalPackets (IntPacket a) (IntPacket b) = compare a b
evalPackets (IntPacket a) b@(ListPacket _) = evalPackets (ListPacket [IntPacket a])  b
evalPackets a@(ListPacket _) (IntPacket b) = evalPackets a (ListPacket [IntPacket b])
evalPackets (ListPacket packets1) (ListPacket packets2) = case (packets1, packets2) of
  ([], []) -> EQ
  ([], _) -> LT
  (_, []) -> GT
  (a : rest1, b : rest2) ->
    let compareFirst = evalPackets a b
    in  if compareFirst == EQ
          then evalPackets (ListPacket rest1) (ListPacket rest2)
          else compareFirst

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