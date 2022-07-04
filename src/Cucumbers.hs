module Cucumbers where

import qualified Data.Set as Set

import Lib (Coord)

solveCucumber' :: FilePath -> IO ()
solveCucumber' fp = do
  cState <- parseCucumberState fp
  let finalState = runStateToEnd cState
  print $ stepNumber finalState

data CucumberState = CucumberState
  { bounds :: (Int, Int)
  , eastHerd :: Set.Set Coord
  , southHerd :: Set.Set Coord
  , stepNumber :: Int
  , prevMoves :: Int
  } deriving (Show)

parseCucumberState :: FilePath -> IO CucumberState
parseCucumberState fp = do
  inputLines <- lines <$> readFile fp
  let bounds = (length inputLines, length (head inputLines))
  let (east, south) = foldl foldCLine (Set.empty, Set.empty) (zip [0,1..] inputLines)
  return $ CucumberState bounds east south 0 (-1)
  where
    foldCLine :: (Set.Set Coord, Set.Set Coord) -> (Int, String) -> (Set.Set Coord, Set.Set Coord)
    foldCLine sets (rowNum, inputLine) = foldl (foldCChar rowNum) sets (zip [0,1..] inputLine)

    foldCChar :: Int -> (Set.Set Coord, Set.Set Coord) -> (Int, Char) -> (Set.Set Coord, Set.Set Coord)
    foldCChar rowNum sets@(east, south) (colNum, c) = case c of
      '.' -> sets
      '>' -> (Set.insert (rowNum, colNum) east, south)
      'v' -> (east, Set.insert (rowNum, colNum) south)
      _ -> error $ "Invalid input char! " ++ [c]

runStateToEnd :: CucumberState -> CucumberState
runStateToEnd cs = if prevMoves cs == 0
  then cs
  else runStateToEnd (runSingleStep cs)

runSingleStep :: CucumberState -> CucumberState
runSingleStep cs@(CucumberState bounds east south stepNumber prev) =
  (CucumberState bounds east2 south2 (stepNumber + 1) m2)
  where
    foldEast :: (Int, Set.Set Coord) -> Coord -> (Int, Set.Set Coord)
    foldEast (m, newSet) oldCoord =
      let next = nextEastCoord oldCoord
      in  if Set.member next east || Set.member next south
            then (m, Set.insert oldCoord newSet)
            else (m + 1, Set.insert next newSet)

    (m1, east2) = foldl foldEast (0, Set.empty) (Set.toList east)

    foldSouth :: Set.Set Coord -> (Int, Set.Set Coord) -> Coord -> (Int, Set.Set Coord)
    foldSouth newEast (m, newSet) oldCoord =
      let next = nextSouthCoord oldCoord
      in  if Set.member next newEast || Set.member next south
            then (m, Set.insert oldCoord newSet)
            else (m + 1, Set.insert next newSet)

    (m2, south2) = foldl (foldSouth east2) (m1, Set.empty) (Set.toList south)

    nextEastCoord (r, c) = (r, (c + 1) `mod` (snd bounds))

    nextSouthCoord (r, c) = ((r + 1) `mod` (fst bounds), c)

basicState :: CucumberState
basicState = CucumberState (4, 10) (Set.fromList [(1, 1), (2,7)]) (Set.fromList [(1,2), (1,7)]) 0 0




