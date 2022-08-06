module Main where

import Data.Array (Array, listArray)
import qualified Data.Array as A
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Test.Tasty
import Test.Tasty.HUnit

import Dijkstra
import Lib (read2DDigits)
import Second (augmentRisks)
import Amphipods

main :: IO ()
main = do
  aocCase1 <- read2DDigits "inputs/day_15_basic.txt"
  aocCase2 <- read2DDigits "inputs/day_15_final.txt"
  let aocCase1' = augmentRisks aocCase1
  let aocCase2' = augmentRisks aocCase2
  defaultMain $ testGroup "Dijkstra Tests"
    (dijkstraTests (Graph2D aocCase1, Graph2D aocCase2, Graph2D aocCase1', Graph2D aocCase2'))

dijkstraTests :: (Graph2D, Graph2D, Graph2D, Graph2D) -> [TestTree]
dijkstraTests (case1, case2, case3, case4) =
  [ testCase "Dijkstra 2D 1" $ findShortestDistance dijkstraInput2D (0, 0) (4, 4) @?= Dist 14
  , testCase "Dijkstra 1" $ findShortestDistance dijkstraInputGraph "A" "D" @?= Dist 40
  , testCase "Dijkstra 2" $ findShortestDistance dijkstraInputGraph2 "A" "D" @?= Dist 32
  , testCase "Dijkstra 3" $ findShortestDistance dijkstraInputGraph2 "A" "F" @?= Infinity
  , testCase "AOC Day 15 - 1" $ findShortestDistance case1 (0,0) (9,9) @?= Dist 40
  , testCase "AOC Day 15 - 2" $ findShortestDistance case2 (0,0) (99,99) @?= Dist 447
  , testCase "AOC Day 15 - 3" $ findShortestDistance case3 (0,0) (49,49) @?= Dist 315
  , testCase "AOC Day 15 - 4" $ findShortestDistance case4 (0,0) (499,499) @?= Dist 2825
  , testCase "AOC Day 23 - 1" $ newSolveAmph aStateTest2 @?= 12521
  , testCase "AOC Day 23 - 2" $ newSolveAmph aState1 @?= 10526
  , testCase "AOC Day 23 - 3" $ newSolveAmph aStateTest3 @?= 44169
  , testCase "AOC Day 23 - 4" $ newSolveAmph aState2 @?= 41284
  , testCase "Dijkstra Path 1" $ findShortestPath_ dijkstraInput2D (0, 0) (4, 4) @?= dijkstraInput2DPath
  , testCase "Dijkstra Path 2" $ findShortestPath_ dijkstraInput2D (2, 2) (2, 2) @?= [(2, 2)]
  , testCase "Dijkstra Path 3" $ findShortestPath_ dijkstraInput2D (2, 2) (2, 1) @?= [(2, 2), (2, 1)]
  , testCase "Dijkstra Path 4" $ findShortestPath_ dijkstraInputGraph "A" "D" @?= ["A", "C", "D"]
  , testCase "Dijkstra Path 5" $ findShortestPath_ dijkstraInputGraph2 "A" "D" @?= ["A", "B", "E", "D"]
  , testCase "Dijkstra Path 6" $ findShortestPath_ dijkstraInputGraph2 "A" "F" @?= []
  , testCase "Dijkstra Path 7" $ findShortestPath_ case1 (0,0) (9,9) @?= aocPath1
  , testCase "Dijkstra Combined" $ findShortestPath case1 (0,0) (9,9) @?= (Dist 40, aocPath1)
  , testCase "Dijkstra Combined" $ findShortestPath dijkstraInputGraph2 "A" "F" @?= (Infinity, [])
  ]

dijkstraInput2D :: Graph2D
dijkstraInput2D = Graph2D $ listArray ((0, 0), (4, 4))
  [ 0, 2, 1, 3, 2
  , 1, 1, 8, 1, 4
  , 1, 8, 8, 8, 1
  , 1, 9, 9, 9, 1
  , 1, 4, 1, 9, 1
  ]

dijkstraInput2DPath :: [(Int, Int)]
dijkstraInput2DPath =
  [ (0, 0)
  , (0, 1)
  , (0, 2)
  , (0, 3)
  , (1, 3)
  , (1, 4)
  , (2, 4)
  , (3, 4)
  , (4, 4)
  ]

dijkstraInputGraph :: Graph
dijkstraInputGraph = Graph $ HM.fromList
  [ ("A", [("D", 100), ("B", 1), ("C", 20)])
  , ("B", [("D", 50)])
  , ("C", [("D", 20)])
  , ("D", [])
  ]

dijkstraInputGraph2 :: Graph
dijkstraInputGraph2 = Graph $ HM.fromList
  [ ("A", [("D", 100), ("B", 1), ("C", 20)])
  , ("B", [("D", 50), ("E", 30)])
  , ("C", [("D", 20)])
  , ("D", [])
  , ("E", [("D", 1)])
  , ("F", [("G", 10)])
  , ("G", [])
  ]

aocPath1 :: [(Int, Int)]
aocPath1 =
  [ (0, 0)
  , (1, 0)
  , (2, 0)
  , (2, 1)
  , (2, 2)
  , (2, 3)
  , (2, 4)
  , (2, 5)
  , (2, 6)
  , (3, 6)
  , (3, 7)
  , (4, 7)
  , (4, 8)
  , (5, 8)
  , (6, 8)
  , (7, 8)
  , (8, 8)
  , (8, 9)
  , (9, 9)
  ]
