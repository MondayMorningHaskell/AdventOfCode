{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import Text.Megaparsec ()
import Text.Megaparsec.Char ()
import Data.Void (Void)
import Data.Text (Text)
import Utils (parseCSVInts, parseFile)

d6ES :: IO (Maybe Integer)
d6ES = solveDay6Easy "inputs/day_6_small.txt"

d6EB :: IO (Maybe Integer)
d6EB = solveDay6Easy "inputs/day_6_big.txt"

d6HS :: IO (Maybe Integer)
d6HS = solveDay6Hard "inputs/day_6_small.txt"

d6HB :: IO (Maybe Integer)
d6HB = solveDay6Hard "inputs/day_6_big.txt"

solveDay6Easy :: String -> IO (Maybe Integer)
solveDay6Easy fp = do
  inputs <- parseInputs fp
  let initialState = produceInitialFishState inputs
  return $ Just (runDay6 80 initialState)

solveDay6Hard :: String -> IO (Maybe Integer)
solveDay6Hard fp = do
  inputs <- parseInputs fp
  let initialState = produceInitialFishState inputs
  return $ Just (runDay6 256 initialState)

parseInputs :: FilePath -> IO [Int]
parseInputs = parseFile parseCSVInts

data FishState = FishState
  { t0 :: Integer
  , t1 :: Integer
  , t2 :: Integer
  , t3 :: Integer
  , t4 :: Integer
  , t5 :: Integer
  , t6 :: Integer
  , t7 :: Integer
  , t8 :: Integer
  } deriving (Show)

produceInitialFishState :: [Int] -> FishState
produceInitialFishState = foldl pifs (FishState 0 0 0 0 0 0 0 0 0)
  where
    pifs :: FishState -> Int -> FishState
    pifs fs i = case i of
      0 -> fs { t0 = t0 fs + 1 }
      1 -> fs { t1 = t1 fs + 1 }
      2 -> fs { t2 = t2 fs + 1 }
      3 -> fs { t3 = t3 fs + 1 }
      4 -> fs { t4 = t4 fs + 1 }
      5 -> fs { t5 = t5 fs + 1 }
      6 -> fs { t6 = t6 fs + 1 }
      7 -> fs { t7 = t7 fs + 1 }
      8 -> fs { t8 = t8 fs + 1 }
      _ -> fs

sumFishState :: FishState -> Integer
sumFishState (FishState f0 f1 f2 f3 f4 f5 f6 f7 f8) = f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8

runDay6 :: Int -> FishState -> Integer
runDay6 numDays initialState = sumFishState finalState
  where
    finalState = last (take (numDays + 1) (iterate updateFishState initialState))

updateFishState :: FishState -> FishState
updateFishState (FishState f0 f1 f2 f3 f4 f5 f6 f7 f8) = FishState
  f1 f2 f3 f4 f5 f6 (f7 + f0) f8 f0