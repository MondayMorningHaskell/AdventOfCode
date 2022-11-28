{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Graph2D where

import qualified Data.Array as A
import Data.Maybe (catMaybes)

import qualified Dijkstra2 as D2
import qualified Dijkstra3 as D3

newtype Graph2D = Graph2D (A.Array (Int, Int) Int)

instance D2.DijkstraGraph Graph2D (Int, Int) Int where
  dijkstraEdges (Graph2D arr) cell = [(n, arr A.! n) | n <- neighbors]
    where
      neighbors = getNeighbors arr cell

instance D3.DijkstraGraph Graph2D where
  type DijkstraNode Graph2D = (Int, Int)
  type DijkstraCost Graph2D = Int
  dijkstraEdges (Graph2D arr) cell = [(n, arr A.! n) | n <- neighbors]
    where
      neighbors = getNeighbors arr cell

getNeighbors :: A.Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
getNeighbors input (row, col) = catMaybes [maybeUp, maybeDown, maybeLeft, maybeRight]
  where
    (maxRow, maxCol) = snd . A.bounds $ input
    maybeUp = if row > 0 then Just (row - 1, col) else Nothing
    maybeDown = if row < maxRow then Just (row + 1, col) else Nothing
    maybeLeft = if col > 0 then Just (row, col - 1) else Nothing
    maybeRight = if col < maxCol then Just (row, col + 1) else Nothing

dijkstraInput2D :: Graph2D
dijkstraInput2D = Graph2D $ A.listArray ((0, 0), (4, 4))
  [ 0, 2, 1, 3, 2
  , 1, 1, 8, 1, 4
  , 1, 8, 8, 8, 1
  , 1, 9, 9, 9, 1
  , 1, 4, 1, 9, 1
  ]

cost2 :: D2.Distance Int
cost2 = D2.findShortestDistance dijkstraInput2D (0 :: Int, 0 :: Int) (4, 4)

cost3 :: D3.Distance Int
cost3 = D3.findShortestDistance dijkstraInput2D (0, 0) (4, 4)