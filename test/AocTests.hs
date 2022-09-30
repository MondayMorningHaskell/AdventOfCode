module Main where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Day1 (solveDay1Easy, solveDay1Hard)
import Day2 (solveDay2Easy, solveDay2Hard)
import Day3 (solveDay3Easy, solveDay3Hard)
import Day4 (solveDay4Easy, solveDay4Hard)
import Day5 (solveDay5Easy, solveDay5Hard)
import Day6 (solveDay6Easy, solveDay6Hard)
import Day7 (solveDay7Easy, solveDay7Hard)
import Day8 (solveDay8Easy, solveDay8Hard)
import Day9 (solveDay9Easy, solveDay9Hard)
import Day10 (solveDay10Easy, solveDay10Hard)
import Day11 (solveDay11Easy, solveDay11Hard)

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
  day61 <- solveDay6Easy "inputs/day_6_small.txt"
  day62 <- solveDay6Easy "inputs/day_6_big.txt"
  day63 <- solveDay6Hard "inputs/day_6_small.txt"
  day64 <- solveDay6Hard "inputs/day_6_big.txt"
  day71 <- solveDay7Easy "inputs/day_7_small.txt"
  day72 <- solveDay7Easy "inputs/day_7_big.txt"
  day73 <- solveDay7Hard "inputs/day_7_small.txt"
  day74 <- solveDay7Hard "inputs/day_7_big.txt"
  day81 <- solveDay8Easy "inputs/day_8_small.txt"
  day82 <- solveDay8Easy "inputs/day_8_big.txt"
  day83 <- solveDay8Hard "inputs/day_8_small.txt"
  day84 <- solveDay8Hard "inputs/day_8_big.txt"
  day91 <- solveDay9Easy "inputs/day_9_small.txt"
  day92 <- solveDay9Easy "inputs/day_9_big.txt"
  day93 <- solveDay9Hard "inputs/day_9_small.txt"
  day94 <- solveDay9Hard "inputs/day_9_big.txt"
  day101 <- solveDay10Easy "inputs/day_10_small.txt"
  day102 <- solveDay10Easy "inputs/day_10_big.txt"
  day103 <- solveDay10Hard "inputs/day_10_small.txt"
  day104 <- solveDay10Hard "inputs/day_10_big.txt"
  day111 <- solveDay11Easy "inputs/day_11_small.txt"
  day112 <- solveDay11Easy "inputs/day_11_big.txt"
  day113 <- solveDay11Hard "inputs/day_11_small.txt"
  day114 <- solveDay11Hard "inputs/day_11_big.txt"
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
    , testCase "Day 4-3" $ day43 @?= Just 1924
    , testCase "Day 4-4" $ day44 @?= Just 9576
    , testCase "Day 5-1" $ day51 @?= Just 5
    , testCase "Day 5-2" $ day52 @?= Just 4655
    , testCase "Day 5-3" $ day53 @?= Just 12
    , testCase "Day 5-4" $ day54 @?= Just 20500
    , testCase "Day 6-1" $ day61 @?= Just 5934
    , testCase "Day 6-2" $ day62 @?= Just 375482
    , testCase "Day 6-3" $ day63 @?= Just 26984457539
    , testCase "Day 6-4" $ day64 @?= Just 1689540415957
    , testCase "Day 7-1" $ day71 @?= Just 37
    , testCase "Day 7-2" $ day72 @?= Just 344735
    , testCase "Day 7-3" $ day73 @?= Just 168
    , testCase "Day 7-4" $ day74 @?= Just 96798233
    , testCase "Day 8-1" $ day81 @?= Just 26
    , testCase "Day 8-2" $ day82 @?= Just 381
    , testCase "Day 8-3" $ day83 @?= Just 61229
    , testCase "Day 8-4" $ day84 @?= Just 1023686
    , testCase "Day 9-1" $ day91 @?= Just 15
    , testCase "Day 9-2" $ day92 @?= Just 570
    , testCase "Day 9-3" $ day93 @?= Just 1134
    , testCase "Day 9-4" $ day94 @?= Just 899392
    , testCase "Day 10-1" $ day101 @?= Just 26397
    , testCase "Day 10-2" $ day102 @?= Just 166191
    , testCase "Day 10-3" $ day103 @?= Just 288957
    , testCase "Day 10-4" $ day104 @?= Just 1152088313
    , testCase "Day 11-1" $ day111 @?= Just 1656
    , testCase "Day 11-2" $ day112 @?= Just 1627
    , testCase "Day 11-3" $ day113 @?= Just 195
    , testCase "Day 11-4" $ day114 @?= Just 329
    ]
