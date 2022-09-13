{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Text.Megaparsec ()
import Text.Megaparsec.Char ()
import Data.Void (Void)
import Data.Text (Text)
import Utils (parseFile, parseCSVInts, OccMap, incKey, emptyOcc)
import qualified Data.Map as M

d7ES :: IO (Maybe Int)
d7ES = solveDay7Easy "inputs/day_7_small.txt"

d7EB :: IO (Maybe Int)
d7EB = solveDay7Easy "inputs/day_7_big.txt"

d7HS :: IO (Maybe Int)
d7HS = solveDay7Hard "inputs/day_7_small.txt"

d7HB :: IO (Maybe Int)
d7HB = solveDay7Hard "inputs/day_7_big.txt"

solveDay7Easy :: String -> IO (Maybe Int)
solveDay7Easy fp = do
  inputs <- parseFile parseCSVInts fp
  let occMap = foldl incKey emptyOcc inputs
  return $ Just (solveWithEnergyFunction easyEnergyFunction occMap)

solveDay7Hard :: String -> IO (Maybe Int)
solveDay7Hard fp = do
  inputs <- parseFile parseCSVInts fp
  let occMap = foldl incKey emptyOcc inputs
  return $ Just (solveWithEnergyFunction hardEnergyFunction occMap)

solveWithEnergyFunction :: (Int -> Int -> Int) -> OccMap Int -> Int
solveWithEnergyFunction energyFunction occMap = if M.null occMap then 0
  else minimum allCosts
  where
    keys = M.keys occMap
    (minIndex, maxIndex) = (minimum keys, maximum keys)

    allCosts = map (findCost energyFunction occMap) [minIndex..maxIndex]

findCost :: (Int -> Int -> Int) -> OccMap Int -> Int -> Int
findCost energyFunction occMap index = M.foldrWithKey sumEnergy 0 occMap
  where
    sumEnergy :: Int -> Word -> Int -> Int
    sumEnergy i numAtI prevCost = prevCost + (fromIntegral numAtI * energyFunction index i)

easyEnergyFunction :: Int -> Int -> Int
easyEnergyFunction x y = abs (x - y)

hardEnergyFunction :: Int -> Int -> Int
hardEnergyFunction x y = triangleNumber (abs (x - y))

triangleNumber :: Int -> Int
triangleNumber n = n * (n + 1) `quot` 2
