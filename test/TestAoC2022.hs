module Main where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5
import qualified Day6 as D6
import qualified Day7 as D7
-- import qualified Day8 as D8
-- import qualified Day9 as D9
-- import qualified Day10 as D10
-- import qualified Day11 as D11
-- import qualified Day12 as D12
-- import qualified Day13 as D13
-- import qualified Day14 as D14
-- import qualified Day15 as D15
-- import qualified Day16 as D16
-- import qualified Day17 as D17
-- import qualified Day18 as D18
-- import qualified Day19 as D19
-- import qualified Day20 as D20
-- import qualified Day21 as D21
-- import qualified Day22 as D22
-- import qualified Day23 as D23
-- import qualified Day24 as D24
-- import qualified Day25 as D25

main :: IO ()
main = do
  day11 <- D1.easySmall
  day12 <- D1.easyLarge
  day13 <- D1.hardSmall
  day14 <- D1.hardLarge
  day21 <- D2.easySmall
  day22 <- D2.easyLarge
  day23 <- D2.hardSmall
  day24 <- D2.hardLarge
  day31 <- D3.easySmall
  day32 <- D3.easyLarge
  day33 <- D3.hardSmall
  day34 <- D3.hardLarge
  day41 <- D4.easySmall
  day42 <- D4.easyLarge
  day43 <- D4.hardSmall
  day44 <- D4.hardLarge
  day51 <- D5.easySmall
  day52 <- D5.easyLarge
  day53 <- D5.hardSmall
  day54 <- D5.hardLarge
  day61 <- D6.easySmall
  day62 <- D6.easyLarge
  day63 <- D6.hardSmall
  day64 <- D6.hardLarge
  day71 <- D7.easySmall
  day72 <- D7.easyLarge
  day73 <- D7.hardSmall
  day74 <- D7.hardLarge
--   day81 <- D8.easySmall
--   day82 <- D8.easyLarge
--   day83 <- D8.hardSmall
--   day84 <- D8.hardLarge
--   day91 <- D9.easySmall
--   day92 <- D9.easyLarge
--   day93 <- D9.hardSmall
--   day94 <- D9.hardLarge
--   day101 <- D10.easySmall
--   day102 <- D10.easyLarge
--   day103 <- D10.hardSmall
--   day104 <- D10.hardLarge
--   day111 <- D11.easySmall
--   day112 <- D11.easyLarge
--   day113 <- D11.hardSmall
--   day114 <- D11.hardLarge
--   day121 <- D12.easySmall
--   day122 <- D12.easyLarge
--   day123 <- D12.hardSmall
--   day124 <- D12.hardLarge
--   day131 <- D13.easySmall
--   day132 <- D13.easyLarge
--   day133 <- D13.hardSmall
--   day134 <- D13.hardLarge
--   day141 <- D14.easySmall
--   day142 <- D14.easyLarge
--   day143 <- D14.hardSmall
--   day144 <- D14.hardLarge
--   day151 <- D15.easySmall
--   day152 <- D15.easyLarge
--   day153 <- D15.hardSmall
--   day154 <- D15.hardLarge
--   day161 <- D16.easySmall
--   day162 <- D16.easyLarge
--   day163 <- D16.hardSmall
--   day164 <- D16.hardLarge
--   day171 <- D17.easySmall
--   day172 <- D17.easyLarge
--   day173 <- D17.hardSmall
--   day174 <- D17.hardLarge
--   day181 <- D18.easySmall
--   day182 <- D18.easyLarge
--   day183 <- D18.hardSmall
--   day184 <- D18.hardLarge
--   day191 <- D19.easySmall
--   day192 <- D19.easyLarge
--   day193 <- D19.hardSmall
--   day194 <- D19.hardLarge
--   day201 <- D20.easySmall
--   day202 <- D20.easyLarge
--   day203 <- D20.hardSmall
--   day204 <- D20.hardLarge
--   day211 <- D21.easySmall
--   day212 <- D21.easyLarge
--   day213 <- D21.hardSmall
--   day214 <- D21.hardLarge
--   day221 <- D22.easySmall
--   day222 <- D22.easyLarge
--   day223 <- D22.hardSmall
--   day224 <- D22.hardLarge
--   day231 <- D23.easySmall
--   day232 <- D23.easyLarge
--   day233 <- D23.hardSmall
--   day234 <- D23.hardLarge
--   day241 <- D24.easySmall
--   day242 <- D24.easyLarge
--   day243 <- D24.hardSmall
--   day244 <- D24.hardLarge
--   day251 <- D25.easySmall
--   day252 <- D25.easyLarge
--   day253 <- D25.hardSmall
--   day254 <- D25.hardLarge
  defaultMain $ testGroup "Advent of Code Tests"
    [ testCase "Day 1-1" $ day11 @?= Just 24000
    , testCase "Day 1-2" $ day12 @?= Just 68775
    , testCase "Day 1-3" $ day13 @?= Just 45000
    , testCase "Day 1-4" $ day14 @?= Just 202585
    , testCase "Day 2-1" $ day21 @?= Just 15
    , testCase "Day 2-2" $ day22 @?= Just 11449
    , testCase "Day 2-3" $ day23 @?= Just 12
    , testCase "Day 2-4" $ day24 @?= Just 13187
    , testCase "Day 3-1" $ day31 @?= Just 157
    , testCase "Day 3-2" $ day32 @?= Just 8018
    , testCase "Day 3-3" $ day33 @?= Just 70
    , testCase "Day 3-4" $ day34 @?= Just 2518
    , testCase "Day 4-1" $ day41 @?= Just 2
    , testCase "Day 4-2" $ day42 @?= Just 584
    , testCase "Day 4-3" $ day43 @?= Just 4
    , testCase "Day 4-4" $ day44 @?= Just 933
    , testCase "Day 5-1" $ day51 @?= Just "CMZ"
    , testCase "Day 5-2" $ day52 @?= Just "ZWHVFWQWW"
    , testCase "Day 5-3" $ day53 @?= Just "MCD"
    , testCase "Day 5-4" $ day54 @?= Just "HZFZCCWWV"
    , testCase "Day 6-1" $ day61 @?= Just 7
    , testCase "Day 6-2" $ day62 @?= Just 1655
    , testCase "Day 6-3" $ day63 @?= Just 19
    , testCase "Day 6-4" $ day64 @?= Just 2665
    , testCase "Day 7-1" $ day71 @?= Just 95437
    , testCase "Day 7-2" $ day72 @?= Just 1908462
    , testCase "Day 7-3" $ day73 @?= Just 24933642
    , testCase "Day 7-4" $ day74 @?= Just 3979145
    -- , testCase "Day 8-1" $ day81 @?= Just 0
    -- , testCase "Day 8-2" $ day82 @?= Just 0
    -- , testCase "Day 8-3" $ day83 @?= Just 0
    -- , testCase "Day 8-4" $ day84 @?= Just 0
    -- , testCase "Day 9-1" $ day91 @?= Just 0
    -- , testCase "Day 9-2" $ day92 @?= Just 0
    -- , testCase "Day 9-3" $ day93 @?= Just 0
    -- , testCase "Day 9-4" $ day94 @?= Just 0
    -- , testCase "Day 10-1" $ day101 @?= Just 0
    -- , testCase "Day 10-2" $ day102 @?= Just 0
    -- , testCase "Day 10-3" $ day103 @?= Just 0
    -- , testCase "Day 10-4" $ day104 @?= Just 0
    -- , testCase "Day 11-1" $ day111 @?= Just 0
    -- , testCase "Day 11-2" $ day112 @?= Just 0
    -- , testCase "Day 11-3" $ day113 @?= Just 0
    -- , testCase "Day 11-4" $ day114 @?= Just 0
    -- , testCase "Day 12-1" $ day121 @?= Just 0
    -- , testCase "Day 12-2" $ day122 @?= Just 0
    -- , testCase "Day 12-3" $ day123 @?= Just 0
    -- , testCase "Day 12-4" $ day124 @?= Just 0
    -- , testCase "Day 13-1" $ day131 @?= Just 0
    -- , testCase "Day 13-2" $ day132 @?= Just 0
    -- , testCase "Day 13-3" $ day133 @?= Just 0
    -- , testCase "Day 13-4" $ day134 @?= Just 0
    -- , testCase "Day 14-1" $ day141 @?= Just 0
    -- , testCase "Day 14-2" $ day142 @?= Just 0
    -- , testCase "Day 14-3" $ day143 @?= Just 0
    -- , testCase "Day 14-4" $ day144 @?= Just 0
    -- , testCase "Day 15-1" $ day151 @?= Just 0
    -- , testCase "Day 15-2" $ day152 @?= Just 0
    -- , testCase "Day 15-3" $ day153 @?= Just 0
    -- , testCase "Day 15-4" $ day154 @?= Just 0
    -- , testCase "Day 16-1" $ day161 @?= Just 0
    -- , testCase "Day 16-2" $ day162 @?= Just 0
    -- , testCase "Day 16-3" $ day163 @?= Just 0
    -- , testCase "Day 16-4" $ day164 @?= Just 0
    -- , testCase "Day 17-1" $ day171 @?= Just 0
    -- , testCase "Day 17-2" $ day172 @?= Just 0
    -- , testCase "Day 17-3" $ day173 @?= Just 0
    -- , testCase "Day 17-4" $ day174 @?= Just 0
    -- , testCase "Day 18-1" $ day181 @?= Just 0
    -- , testCase "Day 18-2" $ day182 @?= Just 0
    -- , testCase "Day 18-3" $ day183 @?= Just 0
    -- , testCase "Day 18-4" $ day184 @?= Just 0
    -- , testCase "Day 19-1" $ day191 @?= Just 0
    -- , testCase "Day 19-2" $ day192 @?= Just 0
    -- , testCase "Day 19-3" $ day193 @?= Just 0
    -- , testCase "Day 19-4" $ day194 @?= Just 0
    -- , testCase "Day 20-1" $ day201 @?= Just 0
    -- , testCase "Day 20-2" $ day202 @?= Just 0
    -- , testCase "Day 20-3" $ day203 @?= Just 0
    -- , testCase "Day 20-4" $ day204 @?= Just 0
    -- , testCase "Day 21-1" $ day211 @?= Just 0
    -- , testCase "Day 21-2" $ day212 @?= Just 0
    -- , testCase "Day 21-3" $ day213 @?= Just 0
    -- , testCase "Day 21-4" $ day214 @?= Just 0
    -- , testCase "Day 22-1" $ day221 @?= Just 0
    -- , testCase "Day 22-2" $ day222 @?= Just 0
    -- , testCase "Day 22-3" $ day223 @?= Just 0
    -- , testCase "Day 22-4" $ day224 @?= Just 0
    -- , testCase "Day 23-1" $ day231 @?= Just 0
    -- , testCase "Day 23-2" $ day232 @?= Just 0
    -- , testCase "Day 23-3" $ day233 @?= Just 0
    -- , testCase "Day 23-4" $ day234 @?= Just 0
    -- , testCase "Day 24-1" $ day241 @?= Just 0
    -- , testCase "Day 24-2" $ day242 @?= Just 0
    -- , testCase "Day 24-3" $ day243 @?= Just 0
    -- , testCase "Day 24-4" $ day244 @?= Just 0
    -- , testCase "Day 25-1" $ day251 @?= Just 0
    -- , testCase "Day 25-2" $ day252 @?= Just 0
    -- , testCase "Day 25-3" $ day253 @?= Just 0
    -- , testCase "Day 25-4" $ day254 @?= Just 0
    ]