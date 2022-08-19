module Main where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Day1 (solveDay1Easy, solveDay1Hard)

main :: IO ()
main = do
  day11 <- solveDay1Easy "inputs/day_1_small.txt"
  day12 <- solveDay1Easy "inputs/day_1_big.txt"
  day13 <- solveDay1Hard "inputs/day_1_small.txt"
  day14 <- solveDay1Hard "inputs/day_1_big.txt"
  defaultMain $ testGroup "Advent of Code Tests"
    [ testCase "Day 1-1" $ day11 @?= 7
    , testCase "Day 1-2" $ day12 @?= 1226
    , testCase "Day 1-3" $ day13 @?= 5
    , testCase "Day 1-4" $ day14 @?= 1252
    ]