module DijkstraMonad where

import qualified Data.Array as A
import Control.Monad.Reader
import Algorithm.Search
import qualified Data.HashMap.Strict as HM
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Foldable (find)

newtype Graph2D = Graph2D (A.Array (Int, Int) Int)

getNeighbors :: A.Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
getNeighbors = undefined

findShortestPath :: Graph2D -> (Int, Int) -> (Int, Int) -> Maybe (Int, [(Int, Int)])
findShortestPath graph start end = runReader
  (dijkstraM neighbors cost (return . (== end)) start)
  graph
  where
    cost :: (Int, Int) -> (Int, Int) -> Reader Graph2D Int
    cost _ b = do
      (Graph2D gr) <- ask
      return $ gr A.! b

    neighbors :: (Int, Int) -> Reader Graph2D [(Int, Int)]
    neighbors source = do
      (Graph2D gr) <- ask
      return $ getNeighbors gr source


newtype Graph = Graph
   { edges :: HM.HashMap String [(String, Int)] }

type Metadata = HM.HashMap String Int
incrementKey :: String -> Metadata -> Metadata
incrementKey k metadata = HM.insert k (count + 1) metadata
  where
    count = fromMaybe 0 (HM.lookup k metadata)

findShortestPath' :: Graph -> String -> String -> Maybe (Int, [String])
findShortestPath' graph start end = evalState
  (dijkstraM neighbors cost (return . (== end)) start)
  HM.empty
  where
    cost :: String -> String -> State Metadata Int
    cost n1 n2 = 
      let assocs = fromMaybe [] (HM.lookup n1 (edges graph))
          costForN2 = find (\(n, _) -> n == n2) assocs
      in  case costForN2 of
            Nothing -> return maxBound
            Just (_, x) -> return x
    neighbors :: String -> State Metadata [String]
    neighbors node = do
      let neighbors = fst <$> fromMaybe [] (HM.lookup node (edges graph))
      metadata <- get
      put $ foldr incrementKey metadata neighbors
      return neighbors
