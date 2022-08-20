module Main where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Day1 (solveDay1Easy, solveDay1Hard)
import Day2 (solveDay2Easy, solveDay2Hard)

main :: IO ()
main = do
  day11 <- solveDay1Easy "inputs/day_1_small.txt"
  day12 <- solveDay1Easy "inputs/day_1_big.txt"
  day13 <- solveDay1Hard "inputs/day_1_small.txt"
  day14 <- solveDay1Hard "inputs/day_1_big.txt"
  day21 <- solveDay2Easy "inputs/day_2_small.txt"
  day22 <- solveDay2Easy "inputs/day_2_big.txt"
  day23 <- solveDay2Hard "inputs/day_2_small.txt"
  day24 <- solveDay2Hard "inputs/day_2_big.txt"
  defaultMain $ testGroup "Advent of Code Tests"
    [ testCase "Day 1-1" $ day11 @?= 7
    , testCase "Day 1-2" $ day12 @?= 1226
    , testCase "Day 1-3" $ day13 @?= 5
    , testCase "Day 1-4" $ day14 @?= 1252
    , testCase "Day 2-1" $ day21 @?= 150
    , testCase "Day 2-2" $ day22 @?= 1524750
    , testCase "Day 2-3" $ day23 @?= 900
    , testCase "Day 2-4" $ day24 @?= 1592426537
    ]