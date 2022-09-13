module Main where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Day1 (solveDay1Easy, solveDay1Hard)
import Day2 (solveDay2Easy, solveDay2Hard)
import Day3 (solveDay3Easy, solveDay3Hard)
import Day4 (solveDay4Easy, solveDay4Hard)
import Day5 (solveDay5Easy, solveDay5Hard)

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
  day31 <- solveDay3Easy "inputs/day_3_small.txt"
  day32 <- solveDay3Easy "inputs/day_3_big.txt"
  day33 <- solveDay3Hard "inputs/day_3_small.txt"
  day34 <- solveDay3Hard "inputs/day_3_big.txt"
  day41 <- solveDay4Easy "inputs/day_4_small.txt"
  day42 <- solveDay4Easy "inputs/day_4_big.txt"
  day43 <- solveDay4Hard "inputs/day_4_small.txt"
  day44 <- solveDay4Hard "inputs/day_4_big.txt"
  day51 <- solveDay5Easy "inputs/day_5_small.txt"
  day52 <- solveDay5Easy "inputs/day_5_big.txt"
  day53 <- solveDay5Hard "inputs/day_5_small.txt"
  day54 <- solveDay5Hard "inputs/day_5_big.txt"
  defaultMain $ testGroup "Advent of Code Tests"
    [ testCase "Day 1-1" $ day11 @?= 7
    , testCase "Day 1-2" $ day12 @?= 1226
    , testCase "Day 1-3" $ day13 @?= 5
    , testCase "Day 1-4" $ day14 @?= 1252
    , testCase "Day 2-1" $ day21 @?= 150
    , testCase "Day 2-2" $ day22 @?= 1524750
    , testCase "Day 2-3" $ day23 @?= 900
    , testCase "Day 2-4" $ day24 @?= 1592426537
    , testCase "Day 3-1" $ day31 @?= 198
    , testCase "Day 3-2" $ day32 @?= 3847100
    , testCase "Day 3-3" $ day33 @?= 230
    , testCase "Day 3-4" $ day34 @?= 16420940 -- Should this be 4105235?
    , testCase "Day 4-1" $ day41 @?= Just 4512
    , testCase "Day 4-2" $ day42 @?= Just 8580
    , testCase "Day 4-4" $ day43 @?= Just 1924
    , testCase "Day 4-4" $ day44 @?= Just 9576
    , testCase "Day 5-1" $ day51 @?= Just 5
    , testCase "Day 5-2" $ day52 @?= Just 4655
    , testCase "Day 5-4" $ day53 @?= Just 12
    , testCase "Day 5-4" $ day54 @?= Just 20500
    ]