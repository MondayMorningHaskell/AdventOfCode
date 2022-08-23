{-# LANGUAGE ScopedTypeVariables #-}

module Day3 where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import qualified Data.Map as M
import Utils (readStringsFromFile, OccMap, incKey, binaryStringToDecimal, countWhere)

d3ES :: IO Int
d3ES = solveDay3Easy "inputs/day_3_small.txt"

d3EB :: IO Int
d3EB = solveDay3Easy "inputs/day_3_big.txt"

d3HS :: IO Int
d3HS = solveDay3Hard "inputs/day_3_small.txt"

d3HB :: IO Int
d3HB = solveDay3Hard "inputs/day_3_big.txt"

solveDay3Easy :: String -> IO Int
solveDay3Easy fp = calculatePowerEasy <$> readStringsFromFile fp

solveDay3Hard :: String -> IO Int
solveDay3Hard fp = do
  inputs <- readStringsFromFile fp
  calculatePowerHard inputs

calculatePowerEasy :: [String] -> Int
calculatePowerEasy stringLines = binaryStringToDecimal gamma * binaryStringToDecimal epsilon
  where
    gamma = produceGamma occMap (length stringLines)
    epsilon = produceEpsilonFromGamma gamma

    occMap :: OccMap Int
    occMap = foldl processString M.empty stringLines

    processString :: OccMap Int -> String -> OccMap Int
    processString prevMap inputLine = foldl processBit prevMap (zip [0,1..] inputLine)

    processBit :: OccMap Int -> (Int, Char) -> OccMap Int
    processBit prevMap (index, char) = case char of
      '1' -> incKey prevMap index
      _ -> prevMap

produceGamma :: OccMap Int -> Int -> String
produceGamma bitMap numStrings = map gammaChar [0..maxIndex]
  where
    maxIndex = fst $ M.findMax bitMap

    gammaChar :: Int -> Char
    gammaChar i = case M.lookup i bitMap of
      Nothing -> '0'
      Just numberOf1s -> if numberOf1s > fromIntegral (numStrings `quot` 2)
        then '1'
        else '0'

produceEpsilonFromGamma :: String -> String
produceEpsilonFromGamma = map flipChar
  where
    flipChar '1' = '0'
    flipChar _ = '1'

calculatePowerHard :: (MonadIO m) => [String] -> m Int
calculatePowerHard inputStrings = do
  oxygenRating <- findRatingString True inputStrings
  co2Rating <- findRatingString False inputStrings
  return $ binaryStringToDecimal oxygenRating * binaryStringToDecimal co2Rating

findRatingString :: forall m. (MonadIO m) => Bool -> [String] -> m String
findRatingString shouldMatchMostCommon inputs = (inputs !!) <$> f indexMap
  where
    indexMap = zip [0,1..] inputs

    f :: [(Int, String)] -> m Int
    f [] = error "Nothing left"
    f [(lastIndex, lastString)] = return lastIndex
    f currentInputs = do
      results <- filterByBitFrequency shouldMatchMostCommon currentInputs
      f results

filterByBitFrequency :: (MonadIO m) => Bool -> [(Int, String)] -> m [(Int, String)]
filterByBitFrequency _ [] = return []
filterByBitFrequency _ [final] = return [final]
filterByBitFrequency shouldMatchMostCommon inputs = do
  let results = filter filterFunc inputs
  liftIO $ print (numberOf1s, numberOf0s, shouldMatchMostCommon)
  return $ map tail' results
  where
    tail' (i, s) = (i, tail s)

    numStrings = length inputs

    numberOf1s :: Int
    numberOf1s = countWhere (\(_, s) -> not (null s) && head s == '1') inputs
    numberOf0s = numStrings - numberOf1s
    mostCommon = if numberOf1s >= numberOf0s then '1' else '0'
    filterFunc :: (Int, String) -> Bool
    -- Should XOR this
    filterFunc i@(_, thisString) =
      (head thisString == mostCommon && shouldMatchMostCommon) ||
      (head thisString /= mostCommon && not shouldMatchMostCommon)