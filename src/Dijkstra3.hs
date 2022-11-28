{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dijkstra3 where

import Data.Hashable (Hashable)
import qualified Data.Heap as H
import Data.Heap (MinPrioHeap)
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)

data Distance a = Dist a | Infinity
  deriving (Show, Eq)

instance (Ord a) => Ord (Distance a) where
  Infinity <= Infinity = True
  Infinity <= Dist x = False
  Dist x <= Infinity = True
  Dist x <= Dist y = x <= y

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

(!??) :: (Hashable k, Eq k) => HashMap k (Distance d) -> k -> Distance d
(!??) distanceMap key = fromMaybe Infinity (HM.lookup key distanceMap)

newtype Graph node cost = Graph
   { edges :: HashMap node [(node, cost)] }

class DijkstraGraph graph where
  type DijkstraNode graph :: *
  type DijkstraCost graph :: *
  dijkstraEdges :: graph -> DijkstraNode graph -> [(DijkstraNode graph, DijkstraCost graph)]

instance DijkstraGraph (Graph String Int) where
    type DijkstraNode (Graph String Int) = String
    type DijkstraCost (Graph String Int) = Int
    dijkstraEdges graph node = fromMaybe [] (HM.lookup node (edges graph))

data DijkstraState node cost = DijkstraState
  { visitedSet :: HashSet node
  , distanceMap :: HashMap node (Distance cost)
  , nodeQueue :: MinPrioHeap (Distance cost) node
  }

findShortestDistance ::
  forall g. (Hashable (DijkstraNode g), Eq (DijkstraNode g), Num (DijkstraCost g), Ord (DijkstraCost g), DijkstraGraph g) =>
  g -> DijkstraNode g -> DijkstraNode g -> Distance (DijkstraCost g)
findShortestDistance graph src dest = processQueue initialState !?? dest
  where
    initialVisited = HS.empty
    initialDistances = HM.singleton src (Dist 0)
    initialQueue = H.fromList [(Dist 0, src)]
    initialState = DijkstraState initialVisited initialDistances initialQueue

    processQueue :: DijkstraState (DijkstraNode g) (DijkstraCost g) -> HashMap (DijkstraNode g) (Distance (DijkstraCost g))
    processQueue ds@(DijkstraState v0 d0 q0) = case H.view q0 of
      Nothing -> d0
      Just ((minDist, node), q1) -> if node == dest then d0
        else if HS.member node v0 then processQueue (ds {nodeQueue = q1})
        else
          -- Update the visited set
          let v1 = HS.insert node v0
          -- Get all unvisited neighbors of our current node
              allNeighbors = dijkstraEdges graph node
              unvisitedNeighbors = filter (\(n, _) -> not (HS.member n v1)) allNeighbors
          -- Fold each neighbor and recursively process the queue
          in  processQueue $ foldl (foldNeighbor node) (DijkstraState v1 d0 q1) unvisitedNeighbors
    foldNeighbor current ds@(DijkstraState v1 d0 q1) (neighborNode, cost) =
      let altDistance = addDist (d0 !?? current) (Dist cost)
      in  if altDistance < d0 !?? neighborNode
            then DijkstraState v1 (HM.insert neighborNode altDistance d0) (H.insert (altDistance, neighborNode) q1)
            else ds

graph1 :: Graph String Int
graph1 = Graph $ HM.fromList
  [ ("A", [("D", 100), ("B", 1), ("C", 20)])
  , ("B", [("D", 50)])
  , ("C", [("D", 20)])
  , ("D", [])
  ]