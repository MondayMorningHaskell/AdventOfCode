{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Dijkstra where

import Data.Array (Array)
import qualified Data.Array as A
import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Heap (MaxHeap, MinHeap, MaxPrioHeap, MinPrioHeap)
import qualified Data.Heap as H
import Data.Maybe (catMaybes)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS

-- 6. Ord instead of Hashable? Benchmark?
-- 7. Allow both Ord or Hashable?
-- 8. Allow returning the full path
-- 9. Debugging

data GraphDist a = Dist a | Infinity
  deriving (Eq, Show)

instance (Ord a) => Ord (GraphDist a) where
  Infinity <= Infinity = True
  Infinity <= _ = False
  (Dist _) <= Infinity = True
  (Dist x) <= (Dist y) = x <= y

addDist :: (Num a) => GraphDist a -> GraphDist a -> GraphDist a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

rawDist :: (Bounded a) => GraphDist a -> a
rawDist (Dist x) = x
rawDist Infinity = maxBound

class DijkstraGraph g where
  type DijkstraNode g :: *
  type DijkstraDistance g :: *
  dijkstraEdges :: g -> DijkstraNode g -> [(DijkstraNode g, (DijkstraDistance g))]

type DijkstraState n d = (HashSet n, MinPrioHeap (GraphDist d) n, HashMap n (GraphDist d))

(!??) :: (Eq a, Hashable a) => HashMap a (GraphDist d) -> a -> (GraphDist d)
(!??) mp key = case mp HM.!? key of
  Nothing -> Infinity
  Just x -> x

findShortestDistance ::
  forall g. (DijkstraGraph g, Eq (DijkstraNode g), Hashable (DijkstraNode g), Num (DijkstraDistance g), Ord (DijkstraDistance g)) =>
  g -> DijkstraNode g -> DijkstraNode g -> GraphDist (DijkstraDistance g)
findShortestDistance graph src dest = processQueue (HS.empty, initialQueue, dist) !?? dest
  where
    dist :: HashMap (DijkstraNode g) (GraphDist (DijkstraDistance g))
    dist = HM.singleton src (Dist 0)

    initialQueue :: MinPrioHeap (GraphDist (DijkstraDistance g)) (DijkstraNode g)
    initialQueue = H.fromList [((Dist 0), src)] -- [(d, node) | (node, d) <- initialDistances]

    processQueue :: DijkstraState (DijkstraNode g) (DijkstraDistance g) -> HM.HashMap (DijkstraNode g) (GraphDist (DijkstraDistance g))
    processQueue (v0, q0, d0) = case H.view q0 of
      Nothing -> d0
      -- While heap is not empty
      -- Withdraw minimum distance/index (d, i@(r, c)) from heap
      Just ((minDist, coord), q1) -> if HS.member coord v0
        then processQueue (v0, q1, d0)
        -- If we have not seen this i already.
        else
          let v1 = HS.insert coord v0
              -- Get all unvisited neighbors of i (j)
              allNeighbors = dijkstraEdges graph coord
              unvisitedNeighbors = filter (\(c, _) -> not (HS.member c v1)) allNeighbors
              -- Update their distances to be the minimum of src->i + i->j or existing dist[j]
              -- Place that back in the heap
          in  processQueue $ foldl (foldNeighbor coord) (v1, q1, d0) unvisitedNeighbors

    foldNeighbor ::
         DijkstraNode g
      -> DijkstraState (DijkstraNode g) (DijkstraDistance g)
      -> (DijkstraNode g, (DijkstraDistance g))
      -> DijkstraState (DijkstraNode g) (DijkstraDistance g)
    foldNeighbor c1 ds@(v, q, d) (c2, distC1ToC2) =
      let altDistance = addDist (d !?? c1) (Dist distC1ToC2)
      in  if altDistance < d !?? c2
            then (v, (H.insert (altDistance, c2) q), (HM.insert c2 altDistance d))
            else ds

-- Examples
newtype Graph = Graph
  { adjacencyList :: HashMap String [(String, Int)]
  }

instance DijkstraGraph Graph where
  type DijkstraNode Graph = String
  type DijkstraDistance Graph = Int
  dijkstraEdges (Graph adjList) nodeName = case HM.lookup nodeName adjList of
    Nothing -> []
    Just x -> x

newtype Graph2D = Graph2D (A.Array (Int, Int) Int)

instance DijkstraGraph Graph2D where
  type DijkstraNode Graph2D = (Int, Int)
  type DijkstraDistance Graph2D = Int
  dijkstraEdges (Graph2D arr) cell = [(n, arr A.! n) | n <- neighbors]
    where
      neighbors = getNeighbors arr cell

getNeighbors :: Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
getNeighbors input (row, col) = catMaybes [maybeUp, maybeDown, maybeLeft, maybeRight]
  where
    (maxRow, maxCol) = snd . A.bounds $ input
    maybeUp = if row > 0 then Just (row - 1, col) else Nothing
    maybeDown = if row < maxRow then Just (row + 1, col) else Nothing
    maybeLeft = if col > 0 then Just (row, col - 1) else Nothing
    maybeRight = if col < maxCol then Just (row, col + 1) else Nothing