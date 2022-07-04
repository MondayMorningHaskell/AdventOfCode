module Amphipods where

import Control.Monad
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import qualified Data.Heap as H
import qualified Data.Map as M
import qualified Data.Set as Set

solveAmph :: IO ()
solveAmph = do
  let (dist, djs, finalState) = dijkstraAmph aState2
  let moves = unwindPath finalState djs
  forM_ moves print
  -- print (H.size $ djQueue djs)
  -- print (M.size $ djDist djs)
  -- print (Set.size $ djVisited djs)
  -- let finalSorted = sortOn snd (M.toList $ djDist djs)
  -- print (last finalSorted)
  print dist
  -- print finalState

data Amph =
  Amber |
  Bronze |
  Copper |
  Desert
  deriving (Show, Eq, Ord)

aState1 :: AmphState
aState1 = AmphState
  [Copper, Bronze] [Amber, Amber] [Desert, Bronze] [Desert, Copper]
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  2

aStateTest :: AmphState
aStateTest = AmphState
  [] [Amber] [Desert, Bronze] [Desert, Copper]
  (Just Copper) (Just Amber) (Just Bronze) Nothing Nothing Nothing Nothing
  2

aStateTest2 :: AmphState
aStateTest2 = AmphState
  [Bronze, Amber] [Copper, Desert] [Bronze, Copper] [Desert, Amber]
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  2

aState2 :: AmphState
aState2 = AmphState
  [Copper, Desert, Desert, Bronze] [Amber, Copper, Bronze, Amber] [Desert, Bronze, Amber, Bronze] [Desert, Amber, Copper, Copper]
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  4

printAStates :: IO ()
printAStates = forM_ (possibleMoves aStateTest) print

data AmphState = AmphState
  { roomA :: [Amph]
  , roomB :: [Amph]
  , roomC :: [Amph]
  , roomD :: [Amph]
  , hall1 :: Maybe Amph
  , hall2 :: Maybe Amph
  , hall4 :: Maybe Amph
  , hall6 :: Maybe Amph
  , hall8 :: Maybe Amph
  , hall10 :: Maybe Amph
  , hall11 :: Maybe Amph
  , roomSize :: Int
  } deriving (Show, Eq, Ord)

possibleMoves :: AmphState -> [(Int, AmphState, String)]
possibleMoves aState@(AmphState rA rB rC rD h1 h2 h4 h6 h8 h10 h11 rs) =
  if not (null invalidMoves)
    then error $ "Invalid move:\n" ++ show aState ++ "\n" ++ show (head invalidMoves)
    else finalMoves
  where
    invalidMoves = filter (\(_, amphS, _) -> isInvalid amphS) finalMoves
    finalMoves = concat [hallwayMoves, aTopMoves, bTopMoves, cTopMoves, dTopMoves]
    -- Try moving the top one in each room.
    -- Try moving each one in the hallway
    h1Move = case h1 of
      Nothing -> Nothing
      Just Amber -> if fullOf Amber rA && areEmpty [h2]
        then Just (2 + (rs - length rA), aState { roomA = (Amber : rA), hall1 = Nothing }, "H1 to A Room")
        else Nothing
      Just Bronze -> if fullOf Bronze rB && areEmpty [h2, h4]
        then Just (10 * (4 + (rs - length rB)), aState { roomB = (Bronze : rB), hall1 = Nothing }, "H1 to B Room")
        else Nothing
      Just Copper -> if fullOf Copper rC && areEmpty [h2, h4, h6]
        then Just (100 * (6 + (rs - length rC)), aState { roomC = (Copper : rC), hall1 = Nothing }, "H1 to C Room")
        else Nothing
      Just Desert -> if fullOf Desert rD && areEmpty [h2, h4, h6, h8]
        then Just (1000 * (8 + (rs - length rD)), aState { roomD = (Desert : rD), hall1 = Nothing }, "H1 to D Room")
        else Nothing
    h2Move = case h2 of
      Nothing -> Nothing
      Just Amber -> if fullOf Amber rA && areEmpty []
        then Just (1 + (rs - length rA), aState { roomA = (Amber : rA), hall2 = Nothing }, "H2 to A Room")
        else Nothing
      Just Bronze -> if fullOf Bronze rB && areEmpty [h4]
        then Just (10 * (3 + (rs - length rB)), aState { roomB = (Bronze : rB), hall2 = Nothing }, "H2 to B Room")
        else Nothing
      Just Copper -> if fullOf Copper rC && areEmpty [h4, h6]
        then Just (100 * (5 + (rs - length rC)), aState { roomC = (Copper : rC), hall2 = Nothing }, "H2 to C Room")
        else Nothing
      Just Desert -> if fullOf Desert rD && areEmpty [h4, h6, h8]
        then Just (1000 * (7 + (rs - length rD)), aState { roomD = (Desert : rD), hall2 = Nothing }, "H2 to D Room")
        else Nothing
    h4Move = case h4 of
      Nothing -> Nothing
      Just Amber -> if fullOf Amber rA && areEmpty []
        then Just (1 + (rs - length rA), aState { roomA = (Amber : rA), hall4 = Nothing }, "H4 to A Room")
        else Nothing
      Just Bronze -> if fullOf Bronze rB && areEmpty []
        then Just (10 * (1 + (rs - length rB)), aState { roomB = (Bronze : rB), hall4 = Nothing }, "H4 to B Room")
        else Nothing
      Just Copper -> if fullOf Copper rC && areEmpty [h6]
        then Just (100 * (3 + (rs - length rC)), aState { roomC = (Copper : rC), hall4 = Nothing }, "H4 to C Room")
        else Nothing
      Just Desert -> if fullOf Desert rD && areEmpty [h6, h8]
        then Just (1000 * (5 + (rs - length rD)), aState { roomD = (Desert : rD), hall4 = Nothing }, "H4 to D Room")
        else Nothing
    h6Move = case h6 of
      Nothing -> Nothing
      Just Amber -> if fullOf Amber rA && areEmpty [h4]
        then Just (3 + (rs - length rA), aState { roomA = (Amber : rA), hall6 = Nothing }, "H6 to A Room")
        else Nothing
      Just Bronze -> if fullOf Bronze rB && areEmpty []
        then Just (10 * (1 + (rs - length rB)), aState { roomB = (Bronze : rB), hall6 = Nothing }, "H6 to B Room")
        else Nothing
      Just Copper -> if fullOf Copper rC && areEmpty []
        then Just (100 * (1 + (rs - length rC)), aState { roomC = (Copper : rC), hall6 = Nothing }, "H6 to C Room")
        else Nothing
      Just Desert -> if fullOf Desert rD && areEmpty [h8]
        then Just (1000 * (3 + (rs - length rD)), aState { roomD = (Desert : rD), hall6 = Nothing }, "H6 to D Room")
        else Nothing
    h8Move = case h8 of
      Nothing -> Nothing
      Just Amber -> if fullOf Amber rA && areEmpty [h4, h6]
        then Just (5 + (rs - length rA), aState { roomA = (Amber : rA), hall8 = Nothing }, "H8 to A Room")
        else Nothing
      Just Bronze -> if fullOf Bronze rB && areEmpty [h6]
        then Just (10 * (3 + (rs - length rB)), aState { roomB = (Bronze : rB), hall8 = Nothing }, "H8 to B Room")
        else Nothing
      Just Copper -> if fullOf Copper rC && areEmpty []
        then Just (100 * (1 + (rs - length rC)), aState { roomC = (Copper : rC), hall8 = Nothing }, "H8 to C Room")
        else Nothing
      Just Desert -> if fullOf Desert rD && areEmpty []
        then Just (1000 * (1 + (rs - length rD)), aState { roomD = (Desert : rD), hall8 = Nothing }, "H8 to D Room")
        else Nothing
    h10Move = case h10 of
      Nothing -> Nothing
      Just Amber -> if fullOf Amber rA && areEmpty [h4, h6, h8]
        then Just (7 + (rs - length rA), aState { roomA = (Amber : rA), hall10 = Nothing }, "H10 to A Room")
        else Nothing
      Just Bronze -> if fullOf Bronze rB && areEmpty [h6, h8]
        then Just (10 * (5 + (rs - length rB)), aState { roomB = (Bronze : rB), hall10 = Nothing }, "H10 to B Room")
        else Nothing
      Just Copper -> if fullOf Copper rC && areEmpty [h8]
        then Just (100 * (3 + (rs - length rC)), aState { roomC = (Copper : rC), hall10 = Nothing }, "H10 to C Room")
        else Nothing
      Just Desert -> if fullOf Desert rD && areEmpty []
        then Just (1000 * (1 + (rs - length rD)), aState { roomD = (Desert : rD), hall10 = Nothing }, "H10 to D Room")
        else Nothing
    h11Move = case h11 of
      Nothing -> Nothing
      Just Amber -> if fullOf Amber rA && areEmpty [h4, h6, h8, h10]
        then Just (8 + (rs - length rA), aState { roomA = (Amber : rA), hall11 = Nothing }, "H11 to A Room")
        else Nothing
      Just Bronze -> if fullOf Bronze rB && areEmpty [h6, h8, h10]
        then Just (10 * (6 + (rs - length rB)), aState { roomB = (Bronze : rB), hall11 = Nothing }, "H11 to B Room")
        else Nothing
      Just Copper -> if fullOf Copper rC && areEmpty [h8, h10]
        then Just (100 * (4 + (rs - length rC)), aState { roomC = (Copper : rC), hall11 = Nothing }, "H11 to C Room")
        else Nothing
      Just Desert -> if fullOf Desert rD && areEmpty [h10]
        then Just (1000 * (2 + (rs - length rD)), aState { roomD = (Desert : rD), hall11 = Nothing }, "H11 to D Room")
        else Nothing
    hallwayMoves = catMaybes [h1Move, h2Move, h4Move, h6Move, h8Move, h10Move, h11Move]

    aTopMoves = if not (null rA) && not (fullOf Amber rA)
      then
        let aType = head rA
            aTopH1 = if areEmpty [h1, h2]
                       then Just (aTCM aType * (3 + (rs - length rA)), aState { roomA = tail rA, hall1 = Just aType }, "A Room to H1")
                       else Nothing
            aTopH2 = if areEmpty [h2]
                       then Just (aTCM aType * (2 + (rs - length rA)), aState { roomA = tail rA, hall2 = Just aType }, "A Room to H2" )
                       else Nothing
            aTopH4 = if areEmpty [h4]
                       then Just (aTCM aType * (2 + (rs - length rA)), aState { roomA = tail rA, hall4 = Just aType }, "A Room to H4" )
                       else Nothing
            aTopH6 = if areEmpty [h4, h6]
                       then Just (aTCM aType * (4 + (rs - length rA)), aState { roomA = tail rA, hall6 = Just aType }, "A Room to H6" )
                       else Nothing
            aTopH8 = if areEmpty [h4, h6, h8]
                       then Just (aTCM aType * (6 + (rs - length rA)), aState { roomA = tail rA, hall8 = Just aType }, "A Room to H8" )
                       else Nothing
            aTopH10 = if areEmpty [h4, h6, h8, h10]
                       then Just (aTCM aType * (8 + (rs - length rA)), aState { roomA = tail rA, hall10 = Just aType }, "A Room to H10" )
                       else Nothing
            aTopH11 = if areEmpty [h4, h6, h8, h10, h11]
                       then Just (aTCM aType * (9 + (rs - length rA)), aState { roomA = tail rA, hall11 = Just aType }, "A Room to H11" )
                       else Nothing
        in  catMaybes [aTopH1, aTopH2, aTopH4, aTopH6, aTopH8, aTopH10, aTopH11]
      else []
    bTopMoves = if not (null rB) && not (fullOf Bronze rB)
      then
        let aType = head rB
            bTopH1 = if areEmpty [h1, h2, h4]
                       then Just (aTCM aType * (5 + (rs - length rB)), aState { roomB = tail rB, hall1 = Just aType }, "B Room to H1" )
                       else Nothing
            bTopH2 = if areEmpty [h2, h4]
                       then Just (aTCM aType * (4 + (rs - length rB)), aState { roomB = tail rB, hall2 = Just aType }, "B Room to H2" )
                       else Nothing
            bTopH4 = if areEmpty [h4]
                       then Just (aTCM aType * (2 + (rs - length rB)), aState { roomB = tail rB, hall4 = Just aType }, "B Room to H4" )
                       else Nothing
            bTopH6 = if areEmpty [h6]
                       then Just (aTCM aType * (2 + (rs - length rB)), aState { roomB = tail rB, hall6 = Just aType }, "B Room to H6" )
                       else Nothing
            bTopH8 = if areEmpty [h6, h8]
                       then Just (aTCM aType * (4 + (rs - length rB)), aState { roomB = tail rB, hall8 = Just aType }, "B Room to H8" )
                       else Nothing
            bTopH10 = if areEmpty [h6, h8, h10]
                       then Just (aTCM aType * (6 + (rs - length rB)), aState { roomB = tail rB, hall10 = Just aType }, "B Room to H10" )
                       else Nothing
            bTopH11 = if areEmpty [h6, h8, h10, h11]
                       then Just (aTCM aType * (7 + (rs - length rB)), aState { roomB = tail rB, hall11 = Just aType }, "B Room to H11" )
                       else Nothing
        in  catMaybes [bTopH1, bTopH2, bTopH4, bTopH6, bTopH8, bTopH10, bTopH11]
      else []
    cTopMoves = if not (null rC) && not (fullOf Copper rC)
      then
        let aType = head rC
            cTopH1 = if areEmpty [h2, h4, h6, h1]
                       then Just (aTCM aType * (7 + (rs - length rC)), aState { roomC = tail rC, hall1 = Just aType }, "C Room to H1" )
                       else Nothing
            cTopH2 = if areEmpty [h4, h6, h2]
                       then Just (aTCM aType * (6 + (rs - length rC)), aState { roomC = tail rC, hall2 = Just aType }, "C Room to H2" )
                       else Nothing
            cTopH4 = if areEmpty [h6, h4]
                       then Just (aTCM aType * (4 + (rs - length rC)), aState { roomC = tail rC, hall4 = Just aType }, "C Room to H4" )
                       else Nothing
            cTopH6 = if areEmpty [h6]
                       then Just (aTCM aType * (2 + (rs - length rC)), aState { roomC = tail rC, hall6 = Just aType }, "C Room to H6" )
                       else Nothing
            cTopH8 = if areEmpty [h8]
                       then Just (aTCM aType * (2 + (rs - length rC)), aState { roomC = tail rC, hall8 = Just aType }, "C Room to H8" )
                       else Nothing
            cTopH10 = if areEmpty [h8, h10]
                       then Just (aTCM aType * (4 + (rs - length rC)), aState { roomC = tail rC, hall10 = Just aType }, "C Room to H10" )
                       else Nothing
            cTopH11 = if areEmpty [h8, h10, h11]
                       then Just (aTCM aType * (5 + (rs - length rC)), aState { roomC = tail rC, hall11 = Just aType }, "C Room to H11" )
                       else Nothing
        in  catMaybes [cTopH1, cTopH2, cTopH4, cTopH6, cTopH8, cTopH10, cTopH11]
      else []
    dTopMoves = if not (null rD) && not (fullOf Desert rD)
      then
        let aType = head rD
            dTopH1 = if areEmpty [h2, h4, h6, h8, h1]
                       then Just (aTCM aType * (9 + (rs - length rD)), aState { roomD = tail rD, hall1 = Just aType }, "D Room to H1" )
                       else Nothing
            dTopH2 = if areEmpty [h4, h6, h8, h2]
                       then Just (aTCM aType * (8 + (rs - length rD)), aState { roomD = tail rD, hall2 = Just aType }, "D Room to H2" )
                       else Nothing
            dTopH4 = if areEmpty [h6, h8, h4]
                       then Just (aTCM aType * (6 + (rs - length rD)), aState { roomD = tail rD, hall4 = Just aType }, "D Room to H4" )
                       else Nothing
            dTopH6 = if areEmpty [h8, h6]
                       then Just (aTCM aType * (4 + (rs - length rD)), aState { roomD = tail rD, hall6 = Just aType }, "D Room to H6" )
                       else Nothing
            dTopH8 = if areEmpty [h8]
                       then Just (aTCM aType * (2 + (rs - length rD)), aState { roomD = tail rD, hall8 = Just aType }, "D Room to H8" )
                       else Nothing
            dTopH10 = if areEmpty [h10]
                       then Just (aTCM aType * (2 + (rs - length rD)), aState { roomD = tail rD, hall10 = Just aType }, "D Room to H10" )
                       else Nothing
            dTopH11 = if areEmpty [h11]
                       then Just (aTCM aType * (3 + (rs - length rD)), aState { roomD = tail rD, hall11 = Just aType }, "D Room to H11" )
                       else Nothing
        in  catMaybes [dTopH1, dTopH2, dTopH4, dTopH6, dTopH8, dTopH10, dTopH11]
      else []

fullOf :: Amph -> [Amph] -> Bool
fullOf aType amphs = all (== aType) amphs

areEmpty :: [Maybe Amph] -> Bool
areEmpty amphs = all (== Nothing) amphs

aTCM :: Amph -> Int
aTCM Amber = 1
aTCM Bronze = 10
aTCM Copper = 100
aTCM Desert = 1000

-- Dijkstra

getDistOrMax :: AmphState -> DistanceMap -> Int
getDistOrMax mp distMap = case M.lookup mp distMap of
  Nothing -> maxBound
  Just d -> d

isDest :: AmphState -> Bool
isDest (AmphState rA rB rC rD h1 h2 h4 h6 h8 h10 h11 _) =
  catMaybes [h1, h2, h4, h6, h8, h10, h11] == [] &&
  fullOf Amber rA &&
  fullOf Bronze rB &&
  fullOf Copper rC &&
  fullOf Desert rD

data DijkstraState = DijkstraState
  { djDist :: DistanceMap
  , djQueue :: MazeHeap
  , djVisited :: MazeSet
  , djPrev :: M.Map AmphState (String, AmphState)
  }

type DistanceMap = M.Map AmphState Int
type HeapCell = (Int, AmphState)
type MazeHeap = H.MinHeap HeapCell
type MazeSet = Set.Set AmphState

dijkstraAmph :: AmphState -> (Int, DijkstraState, AmphState)
dijkstraAmph start = dijkstraTail (DijkstraState d0 q0 v0 M.empty)
  where
    d0 = M.singleton start 0
    q0 = H.fromList (map (\(c, d) -> (d, c)) (M.toList d0))
    v0 = Set.empty

    dijkstraTail :: DijkstraState -> (Int, DijkstraState, AmphState)
    dijkstraTail djs@(DijkstraState d1 q1 v1 p1) = case H.view q1 of
      Nothing -> (maxBound, djs, start)
      Just ((distU, nextState), restHeap) -> if isDest nextState
        then (distU, djs, nextState)
        else if not (Set.member nextState v1)
          then
            let v2 = Set.insert nextState v1
                neighbors = possibleMoves nextState
                newNeighbors = filter (\(_, n, _) -> not (Set.member n v2)) neighbors
                djs2 = foldl (dijkstraFold nextState) (djs{djVisited = v2, djQueue=restHeap}) newNeighbors
            in dijkstraTail djs2
          else dijkstraTail (DijkstraState d1 restHeap v1 p1)

    dijkstraFold :: AmphState -> DijkstraState -> (Int, AmphState, String) -> DijkstraState
    dijkstraFold stateFromHeapU djs@(DijkstraState d1 q1 v1 p1) (marginalDist, potentialStateV, moveString) =
      let d1U = getDistOrMax stateFromHeapU d1
          alt = if d1U == maxBound then maxBound else d1U + marginalDist
          d2 = M.insert potentialStateV alt d1
          q2 = H.insert (alt, potentialStateV) q1
          p2 = M.insert potentialStateV (moveString, stateFromHeapU) p1
      in  if alt < getDistOrMax potentialStateV d1
            then DijkstraState d2 q2 v1 p2
            else djs

isInvalid :: AmphState -> Bool
isInvalid (AmphState rA rB rC rD h1 h2 h4 h6 h8 h10 h11 rs) =
  (inHalls + inRooms) /= (4 * rs)
  where
    inHalls = length $ catMaybes [h1, h2, h4, h6, h8, h10, h11]
    inRooms = length rA + length rB + length rC + length rD

unwindPath :: AmphState -> DijkstraState -> [String]
unwindPath finalState (DijkstraState _ _ _ prevMap) = unwind' finalState []
  where
    unwind' :: AmphState -> [String] -> [String]
    unwind' aState accum = case M.lookup aState prevMap of
      Nothing -> accum
      Just (moveString, prevState) -> unwind' prevState (moveString : accum)
