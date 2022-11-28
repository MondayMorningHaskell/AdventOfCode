module DijkstraLib where

import qualified Data.Array as A
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes, fromMaybe)
import Algorithm.Search (dijkstra, dijkstraAssoc)

data Distance a = Dist a | Infinity
  deriving (Show)

newtype Graph = Graph
   { edges :: HashMap String [(String, Int)] }

graph1 :: Graph
graph1 = Graph $ HM.fromList
  [ ("A", [("D", 100), ("B", 1), ("C", 20)])
  , ("B", [("D", 50)])
  , ("C", [("D", 20)])
  , ("D", [])
  ]

findShortestDistance :: Graph -> String -> String -> Distance Int
findShortestDistance graph start end = case answer of
  Just (dist, path) -> Dist dist
  Nothing -> Infinity
  where
    costFunction node = fromMaybe [] (HM.lookup node (edges graph))
    answer = dijkstraAssoc costFunction (== end) start

newtype Graph2D = Graph2D (A.Array (Int, Int) Int)

getNeighbors :: A.Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
getNeighbors input (row, col) = catMaybes [maybeUp, maybeDown, maybeLeft, maybeRight]
  where
    (maxRow, maxCol) = snd . A.bounds $ input
    maybeUp = if row > 0 then Just (row - 1, col) else Nothing
    maybeDown = if row < maxRow then Just (row + 1, col) else Nothing
    maybeLeft = if col > 0 then Just (row, col - 1) else Nothing
    maybeRight = if col < maxCol then Just (row, col + 1) else Nothing

graph2d :: Graph2D
graph2d = Graph2D $ A.listArray ((0, 0), (4, 4))
  [ 0, 2, 1, 3, 2
  , 1, 1, 8, 1, 4
  , 1, 8, 8, 8, 1
  , 1, 9, 9, 9, 1
  , 1, 4, 1, 9, 1
  ]

findShortestPath2D :: Graph2D -> (Int, Int) -> (Int, Int) -> Maybe (Int, [(Int, Int)])
findShortestPath2D (Graph2D graph) start end = dijkstra
  (getNeighbors graph)
  (\_ b -> graph A.! b)
  (== end)
  start
