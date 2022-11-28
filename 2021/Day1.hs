module Day1 where

import Utils (readIntsFromFile, countWhere)

d1ES :: IO Int
d1ES = solveDay1Easy "inputs/day_1_small.txt"

d1EB :: IO Int
d1EB = solveDay1Easy "inputs/day_1_big.txt"

d1HS :: IO Int
d1HS = solveDay1Hard "inputs/day_1_small.txt"

d1HB :: IO Int
d1HB = solveDay1Hard "inputs/day_1_big.txt"

solveDay1Easy :: String -> IO Int
solveDay1Easy fp = findIncreases <$> readIntsFromFile fp

solveDay1Hard :: String -> IO Int
solveDay1Hard fp = findIncreases . build3Windows <$> readIntsFromFile fp

findIncreases :: [Int] -> Int
findIncreases [] = 0
findIncreases depths = countWhere firstSmaller (zip depths (tail depths))
  where
    firstSmaller (x, y) = x < y

build3Windows :: [Int] -> [Int]
build3Windows inputs = if length inputs < 3 then []
  else zipWith (+) (zipWith (+) inputs (drop 1 inputs)) (drop 2 inputs)