module Third where

import Control.Monad.State
import Data.List.Split (splitOn)
import qualified Data.Heap as H
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import System.Environment

import Second (addForKey, Coord3)

solveDirac :: IO ()
solveDirac = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveDirac' fp

solveDirac' :: FilePath -> IO ()
solveDirac' fp = do
  starts@(p1Start, p2Start) <- readStartPositions fp
  -- print starts
  -- let gs0 = GameState p1Start p2Start 0 0 1 Player1 0
  -- let finalState = execState playGameRound gs0
  -- let losingScore = min (p1Score finalState) (p2Score finalState)
  -- print $ losingScore * dieRolls finalState
  let qs0 = QS (M.singleton (p1Start, p2Start, 0, 0, True) 1) 0 0
  print (runQuantumState qs0)

readStartPositions :: FilePath -> IO (Int, Int)
readStartPositions fp = do
  (first : second : _) <- lines <$> readFile fp
  let p1Start = read (drop 28 first)
  let p2Start = read (drop 28 second)
  return (p1Start, p2Start)

data Player = Player1 | Player2
  deriving (Show)

data GameState = GameState
  { p1Position :: Int
  , p2Position :: Int
  , p1Score :: Int
  , p2Score :: Int
  , nextDieRoll :: Int
  , currentPlayer :: Player
  , dieRolls :: Int
  } deriving (Show)

playGameRound1 :: State GameState ()
playGameRound1 = do
  (n1, n2, n3) <- rollDice
  gs <- get
  case currentPlayer gs of
    Player1 -> do
      let newPos = adjustNumber (p1Position gs + n1 + n2 + n3) 10
      let newScore = p1Score gs + newPos
      put $ gs 
           { p1Position = newPos
           , p1Score = newScore
           , currentPlayer = Player2
           }
      if newScore >= 1000 then return () else return ()
    Player2 -> do
      let newPos = adjustNumber (p2Position gs + n1 + n2 + n3) 10
      let newScore = p2Score gs + newPos
      put $ gs 
           { p2Position = newPos
           , p2Score = newScore
           , currentPlayer = Player1
           }
      if newScore >= 1000 then return () else return ()

playGameRound :: State GameState ()
playGameRound = do
  (n1, n2, n3) <- rollDice
  gs <- get
  case currentPlayer gs of
    Player1 -> do
      let newPos = adjustNumber (p1Position gs + n1 + n2 + n3) 10
      let newScore = p1Score gs + newPos
      put $ gs 
           { p1Position = newPos
           , p1Score = newScore
           , currentPlayer = Player2
           }
      if newScore >= 1000 || dieRolls gs >= 993 then return () else playGameRound
    Player2 -> do
      let newPos = adjustNumber (p2Position gs + n1 + n2 + n3) 10
      let newScore = p2Score gs + newPos
      put $ gs 
           { p2Position = newPos
           , p2Score = newScore
           , currentPlayer = Player1
           }
      if newScore >= 1000 || dieRolls gs >= 993 then return () else playGameRound

playGameRoundOver95 :: State GameState ()
playGameRoundOver95 = do
  (n1, n2, n3) <- rollDice
  gs <- get
  case currentPlayer gs of
    Player1 -> do
      let newPos = adjustNumber (p1Position gs + n1 + n2 + n3) 10
      let newScore = p1Score gs + newPos
      put $ gs 
           { p1Position = newPos
           , p1Score = newScore
           , currentPlayer = Player2
           }
      if dieRolls gs >= 95 then return () else playGameRoundOver95
    Player2 -> do
      let newPos = adjustNumber (p2Position gs + n1 + n2 + n3) 10
      let newScore = p2Score gs + newPos
      put $ gs 
           { p2Position = newPos
           , p2Score = newScore
           , currentPlayer = Player1
           }
      if dieRolls gs >= 95 then return () else playGameRoundOver95

rollDice :: State GameState (Int, Int, Int)
rollDice = do
  gs <- get
  let nextRoll = nextDieRoll gs
  let newNext = adjustNumber (nextRoll + 3) 100
  put $ gs { nextDieRoll = newNext, dieRolls = dieRolls gs + 3 }
  return
    ( adjustNumber nextRoll 100
    , adjustNumber (nextRoll + 1) 100
    , adjustNumber (nextRoll + 2) 100)

-- 9 10 -> 9
-- 11 10 -> 1
-- 2 10 -> 2
-- 21 10 -> 1
-- 103 100 -> 3
-- 315 10 -> 5
adjustNumber :: Int -> Int -> Int
adjustNumber num max' = if num > max'
  then if modded == 0 then max' else modded
  else num
  where
    modded = num `mod` max'

type QGameState =
  ( Int -- Player 1 Position
  , Int -- Player 2 Position
  , Int -- Player 1 Score
  , Int -- Player 2 Score
  , Bool -- Player 1 is on move?
  )

type QuantumMap = M.Map QGameState Integer
data Result = Player1Win | Player2Win
data FullQuantumState = QS
  { qMap :: QuantumMap
  , p1Wins :: Integer
  , p2Wins :: Integer
  } deriving (Show)

runQuantumState :: FullQuantumState -> (Integer, FullQuantumState)
runQuantumState fqs = if M.null (qMap fqs)
  then (max (p1Wins fqs) (p2Wins fqs), newState)
  else runQuantumState newState
  where
    newFqs0 = fqs { qMap = M.empty }
    allCurrentStates = M.toList (qMap fqs)

    newState = foldl qsFold newFqs0 allCurrentStates

    qsFold :: FullQuantumState -> (QGameState, Integer) -> FullQuantumState
    qsFold fullState (qs, count) =
      let outcomes = possibleOutcomes qs
      in  foldl (outcomeFold count) fullState outcomes

    outcomeFold :: Integer -> FullQuantumState ->
                Either QGameState Result -> FullQuantumState
    outcomeFold count fqs' (Right Player1Win) =
      fqs' { p1Wins = p1Wins fqs' + count }
    outcomeFold count fqs' (Right Player2Win) =
      fqs' { p2Wins = p2Wins fqs' + count }
    outcomeFold count fqs' (Left newState) =
      fqs' { qMap = addForKey newState count (qMap fqs') }

possibleOutcomes :: QGameState -> [Either QGameState Result]
possibleOutcomes qs0 = (updateState qs0) <$> allRolls
  where
    allRolls = [(x, y, z) | x <- [1..3], y <- [1..3], z <- [1..3]]
    

updateState :: QGameState -> (Int, Int, Int) -> Either QGameState Result
updateState
  (p1Pos, p2Pos, p1Score, p2Score, p1OnMove) (r1, r2, r3) =
  if p1OnMove
    then if p1Wins then Right Player1Win
      else Left (newP1Pos, p2Pos, newP1Score, p2Score, False)
    else if p2Wins then Right Player2Win
      else Left (p1Pos, newP2Pos, p1Score, newP2Score, True)
  where
    newP1Pos = adjustNumber (r1 + r2 + r3 + p1Pos) 10
    newP2Pos = adjustNumber (r1 + r2 + r3 + p2Pos) 10
    newP1Score = p1Score + newP1Pos
    newP2Score = p2Score + newP2Pos
    p1Wins = newP1Score >= 21
    p2Wins = newP2Score >= 21

-- Day 22

solveReboot = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveReboot' fp

solveReboot' :: FilePath -> IO ()
solveReboot' fp = do
  cubes <- parseRebootCubes fp
  -- let cubes' = filter isSmall cubes
  let finalCubes = foldl foldNewCube (CubeSet []) cubes
  -- let finalCubes = foldl foldCubeB (Set.empty) cubes'
  -- print finalCubes
  print $ countOn finalCubes

isSmall :: Cube -> Bool
isSmall (Cube x1 x2 y1 y2 z1 z2 _) =
  x1 >= -50 && x2 <= 50 &&
  y1 >= -50 && y2 <= 50 &&
  z1 >= -50 && z2 <= 50

data Cube = Cube
  { xMin :: Int
  , xMax :: Int
  , yMin :: Int
  , yMax :: Int
  , zMin :: Int
  , zMax :: Int
  , isOn :: Bool
  } deriving (Show)

newtype CubeSet = CubeSet [Cube]
  deriving (Show)

parseRebootCubes :: FilePath -> IO [Cube]
parseRebootCubes fp = (map parseCube . lines) <$> readFile fp

parseCube :: String -> Cube
parseCube input = case take 3 input of
  "on " -> (mkCube True) (parseCube (drop 3 input))
  "off" -> (mkCube False) (parseCube (drop 4 input))
  _ -> error $ "Invalid input " ++ input
  where
    parseCube cubeLine =
      let [xs, ys, zs] = splitOn "," cubeLine
          [x1, x2] = splitOn ".." (drop 2 xs)
          [y1, y2] = splitOn ".." (drop 2 ys)
          [z1, z2] = splitOn ".." (drop 2 zs)
      in  read <$> [x1, x2, y1, y2, z1, z2]

    mkCube :: Bool -> [Int] -> Cube
    mkCube on [x1, x2, y1, y2, z1, z2] = Cube
      x1 x2 y1 y2 z1 z2 on

-- Invariant: No two cubes contain any overlapping points.
foldNewCube :: CubeSet -> Cube -> CubeSet
foldNewCube (CubeSet prevCubes) newCube =
  (CubeSet $ beforeAppending ++ (reverse (foldl cubesToAdd [] prevCubes)))
  where
    cubesToAdd :: [Cube] -> Cube -> [Cube]
    cubesToAdd prev oldCube = if not (cubesIntersect oldCube newCube)
      then prev
      else
       let intersectCube = cubeIntersection oldCube newCube
       in  intersectCube { isOn = not (isOn oldCube) } : prev

    beforeAppending = if isOn newCube
      then prevCubes ++ [newCube]
      else prevCubes

cubesIntersect :: Cube -> Cube -> Bool
cubesIntersect
  c1@(Cube mnx1 mxx1 mny1 mxy1 mnz1 mxz1 _)
  c2@(Cube mnx2 mxx2 mny2 mxy2 mnz2 mxz2 _) = not $
      (mxx1 < mnx2 || mxx2 < mnx1) &&
      (mxy1 < mny2 || mxy2 < mny1) &&
      (mxz1 < mnz2 || mxz2 < mnz1)

cubeIntersection :: Cube -> Cube -> Cube
cubeIntersection
  c1@(Cube mnx1 mxx1 mny1 mxy1 mnz1 mxz1 _)
  c2@(Cube mnx2 mxx2 mny2 mxy2 mnz2 mxz2 on2) = newCube
  where
    newMnx = max mnx1 mnx2
    newMxx = min mxx1 mxx2
    newMny = max mny1 mny2
    newMxy = min mxy1 mxy2
    newMnz = max mnz1 mnz2
    newMxz = min mxz1 mxz2
    newCube = Cube newMnx newMxx newMny newMxy newMnz newMxz on2

-- c1 = 1 1 1 1 1 1
-- c2 = 0 2 0 2 0 2

divideCube :: Cube -> Cube -> [Cube] -> [Cube]
divideCube 
  newCube@(Cube mnx1 mxx1 mny1 mxy1 mnz1 mxz1 on1)
  oldCube@(Cube mnx2 mxx2 mny2 mxy2 mnz2 mxz2 on2)
  accum = foldl (\p n -> n : p) accum (filter isValid allCubes)
  where
    allCubes = [ c1, c2, c3, c4, c5, c6 ]
    -- Above  (z >)
    c1 = Cube mnx2 mxx2 mny2 mxy2 (mxz1 + 1) mxz2 on2
    -- Below  (z <)
    c2 = Cube mnx2 mxx2 mny2 mxy2 mnz2 (mnz1 - 1) on2
    -- Right  (y >, z <= <=)
    c3 = Cube mnx2 mxx2 (mxy1 + 1) mxy2 mnz1 mxz1 on2
    -- Left   (y <, z <= <=)
    c4 = Cube mnx2 mxx2 mny2 (mny1 - 1) mnz1 mxz1 on2
    -- Behind (x >, z ==, y ==)
    c5 = Cube (mxx1 + 1) mxx2 mny1 mxy1 mnz1 mxz1 on2
    -- Front  (x <, z ==, y ==)
    c6 = Cube mnx2 (mxx1 - 1) mny1 mxy1 mnz1 mxz1 on2
    {-
    -- All 3 dimensions less
    c1 = Cube mnx2 (mnx1 - 1) mny2 (mny1 - 1) mnz2 (mnz1 - 1) on2
    -- 2 Dimensions less, one equal
    c2 = Cube mnx2 (mnx1 - 1) mny2 (mny1 - 1) mnz1 mxz1 on2
    c3 = Cube mnx2 (mnx1 - 1) mny1 mxy1 mnz2 (mnz1 - 1) on2
    c4 = Cube mnx1 mxx1 mny2 (mny1 - 1) mnz2 (mnz1 - 1) on2
    -- 2 Dimensions less, one more
    c5 = Cube mnx2 (mnx1 - 1) mny2 (mny1 - 1) (mxz1 + 1) mxz2 on2
    c6 = Cube mnx2 (mnx1 - 1) (mxy1 + 1) mxy2 mnz2 (mnz1 - 1) on2
    c7 = Cube (mxx1 + 1) mxx2 mny2 (mny1 - 1) mnz2 (mnz1 - 1) on2
    -- 1 Dimension less, two equal
    c8 = Cube mnx2 (mnx1 - 1) mny1 mxy1 mnz1 mxz1 on2
    c9 = Cube mnx1 mxx1 mny1 mxy1 mnz2 (mnz1 - 1) on2
    c10 = Cube mnx1 mxx1 mny2 (mny1 - 1) mnz1 mxz1 on2
    -- 1 Dimension less, one equal, one more
    c11 = Cube mnx2 (mnx1 - 1) mny1 mxy1 (mxz1 + 1) mxz2 on2
    c12 = Cube mnx2 (mnx1 - 1) (mxy1 + 1) mxy2 mnz1 mxz1 on2
    c13 = Cube mnx1 mxx1 mny2 (mny1 - 1) (mxz1 + 1) mxz2 on2
    c14 = Cube mnx1 mxx1 (mxy1 + 1) mxy2 mnz2 (mnz1 - 1) on2
    c15 = Cube (mxx1 + 1) mxx2 mny1 mxy1 mnz2 (mnz1 - 1) on2
    c16 = Cube (mxx1 + 1) mxx2 mny2 (mny1 - 1) mnz1 mxz1 on2
    -- 1 Dimension more, two equal
    c18 = Cube (mxx1 + 1) mxx2 mny1 mxy1 mnz1 mxz1 on2
    c19 = Cube mnx1 mxx1 mny1 mxy1 (mxz1 + 1) mxz2 on2
    c20 = Cube mnx1 mxx1 (mxy1 + 1) mxy2 mnz1 mxz1 on2
    -- 2 Dimensions more, one less
    c21 = Cube mnx2 (mnx1 - 1) (mxy1 + 1) mxy2 (mxz1 + 1) mxz2 on2
    c22 = Cube (mxx1 + 1) mxx2 mny2 (mny1 - 1) (mxz1 + 1) mxz2 on2
    c23 = Cube (mxx1 + 1) mxx2 (mxy1 + 1) mxy2 mnz2 (mnz1 - 1) on2
    -- 2 Dimensions more, one equal
    c24 = Cube mnx1 mxx1 (mxy1 + 1) mxy2 (mxz1 + 1) mxz2 on2
    c25 = Cube (mxx1 + 1) mxx2 mny1 mxy1 (mxz1 + 1) mxz2 on2
    c26 = Cube (mxx1 + 1) mxx2 (mxy1 + 1) mxy2 mnz1 mxz1 on2
    -- All 3 dimensions more
    c27 = Cube (mxx1 + 1) mxx2 (mxy1 + 1) mxy2 (mxz1 + 1) mxz2 on2
    -}

isValid :: Cube -> Bool
isValid (Cube mnx mxx mny mxy mnz mxz _) =
  mnx <= mxx && mny <= mxy && mnz <= mxz

countOn :: CubeSet -> Integer
countOn (CubeSet cubes) = foldl foldCube 0 cubes
  where
    foldCube :: Integer -> Cube -> Integer
    foldCube prev (Cube mnx mxx mny mxy mnz mxz on) =
      let results = fromIntegral
            ( (mxx - mnx + 1)
            * (mxy - mny + 1)
            * (mxz - mnz + 1)
            )
     in  if on then prev + results else prev - results

-- Day 22 (Basic)

foldCubeB :: Set.Set Coord3 -> Cube -> Set.Set Coord3
foldCubeB prevSet (Cube x1 x2 y1 y2 z1 z2 on) = foldl foldCoord prevSet allCombos
  where
    allCombos = [(x, y, z) | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]
    
    foldCoord :: Set.Set Coord3 -> Coord3 -> Set.Set Coord3
    foldCoord set0 c3 = if on
      then Set.insert c3 set0
      else Set.delete c3 set0


-- Day 23

data MazePosition =
  H1 |
  H2 |
  H4 |
  H6 |
  H8 |
  H10 |
  H11 |
  R11 |
  R12 |
  R21 |
  R22 |
  R31 |
  R32 |
  R41 |
  R42
  deriving (Show, Ord, Eq)

data AmphType = Amber | Bronze | Copper | Desert

data MazeState = MazeState
  { a1Pos :: MazePosition
  , a2Pos :: MazePosition
  , b1Pos :: MazePosition
  , b2Pos :: MazePosition
  , c1Pos :: MazePosition
  , c2Pos :: MazePosition
  , d1Pos :: MazePosition
  , d2Pos :: MazePosition
  } deriving (Show, Ord, Eq)

type HeapCell = (Int, MazeState)
type MazeHeap = H.MinHeap HeapCell
type DistanceMap = M.Map MazeState Int
type MazeSet = Set.Set MazeState

data DijkstraState = DijkstraState
  { djDist :: DistanceMap
  , djQueue :: MazeHeap
  , djVisited :: MazeSet
  }

type FoldLists =
  ( [MazePosition]
  , [MazePosition]
  , [MazePosition]
  , [MazePosition]
  )

parseInitialMazeState :: FilePath -> IO MazeState
parseInitialMazeState fp = do
  inputLines <- lines <$> readFile fp
  let rooms1 = inputLines !! 2
  let rooms2 = inputLines !! 3
  let ordering1 = filter (/= "") (splitOn "#" rooms1)
  let ordering2 = filter (/= "") (tail (splitOn "#" rooms2))
  print (ordering1, ordering2)
  let finalSets = foldl (parseFold ordering1 ordering2) ([], [], [], []) [0..3]
  let ([a1, a2], [b1, b2], [c1, c2], [d1, d2]) = finalSets
  return $ MazeState a1 a2 b1 b2 c1 c2 d1 d2
  where
    parseFold :: [String] -> [String] -> FoldLists -> Int -> FoldLists
    parseFold o1 o2 (aps, bps, cps, dps) i =
      let (rp1, rp2) = case i of
                        0 -> (R11, R12)
                        1 -> (R21, R22)
                        2 -> (R31, R32)
                        3 -> (R41, R42)
          (s1, s2) = (o1 !! i, o2 !! i)
          (aps2, bps2, cps2, dps2) = case s1 of
                       "A" -> (rp1 : aps, bps, cps, dps)
                       "B" -> (aps, rp1 : bps, cps, dps)
                       "C" -> (aps, bps, rp1 : cps, dps)
                       "D" -> (aps, bps, cps, rp1 : dps)
          newFoldList3 = case s2 of
                       "A" -> (rp2 : aps2, bps2, cps2, dps2)
                       "B" -> (aps2, rp2 : bps2, cps2, dps2)
                       "C" -> (aps2, bps2, rp2 : cps2, dps2)
                       "D" -> (aps2, bps2, cps2, rp2 : dps2)
                       _ -> error $ "Invalid s2 " ++ s2
      in  newFoldList3

solveAmphipods = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveAmphipods' fp

solveAmphipods' :: FilePath -> IO ()
solveAmphipods' fp = do
  ims <- parseInitialMazeState fp
  print ims

getDistOrMax :: MazeState -> DistanceMap -> Int
getDistOrMax mp distMap = case M.lookup mp distMap of
  Nothing -> maxBound
  Just d -> d

dijkstraAmph :: MazeState -> Int
dijkstraAmph start = dijkstraTail (DijkstraState d0 q0 v0)
  where
    d0 = M.singleton start 0
    q0 = H.fromList (map (\(c, d) -> (d, c)) (M.toList d0))
    v0 = Set.singleton start

    dijkstraTail :: DijkstraState -> Int
    dijkstraTail djs@(DijkstraState d1 q1 v1) = case H.view q1 of
      Nothing -> maxBound
      Just ((distU, nextState), restHeap) -> if isDest nextState
        then distU
        else if not (Set.member nextState v1)
          then
            let v2 = Set.insert nextState v1
                neighbors = getPossibleMoves nextState
                newNeighbors = filter (\(_, n) -> not (Set.member n v2)) neighbors
                djs2 = foldl (dijkstraFold nextState) (djs{djVisited = v2, djQueue=restHeap}) newNeighbors
            in dijkstraTail djs2
          else dijkstraTail (DijkstraState d1 restHeap v1)

    dijkstraFold :: MazeState -> DijkstraState -> (Int, MazeState) -> DijkstraState
    dijkstraFold stateFromHeapU djs@(DijkstraState d1 q1 v1) (marginalDist, potentialStateV) =
      let d1U = getDistOrMax stateFromHeapU d1
          alt = if d1U == maxBound then maxBound else d1U + marginalDist
          d2 = M.insert potentialStateV alt d1
          q2 = H.insert (alt, potentialStateV) q1
      in  if alt < getDistOrMax potentialStateV d1
            then DijkstraState d2 q2 v1
            else djs

isDest :: MazeState -> Bool
isDest (MazeState a1 a2 b1 b2 c1 c2 d1 d2) =
  (a1 == R11 || a1 == R12) &&
  (a2 == R11 || a2 == R12) &&
  (b1 == R21 || b1 == R22) &&
  (b2 == R21 || b2 == R22) &&
  (c1 == R31 || c1 == R32) &&
  (c2 == R31 || c2 == R32) &&
  (d1 == R41 || d1 == R42) &&
  (d2 == R41 || d2 == R42)

getPossibleMoves :: MazeState -> [(Int, MazeState)]
getPossibleMoves ms@(MazeState a1 a2 b1 b2 c1 c2 d1 d2) = allMoves
  where
    occupied = Set.fromList [a1, a2, b1, b2, c1, c2, d1, d2]
    allMoves = concat [a1Moves, a2Moves, b1Moves, b2Moves, c1Moves, c2Moves, d1Moves, d2Moves]

    a1Moves :: [(Int, MazeState)]
    a1Moves = if Set.member a1 hallwayPositions
      then
        let moves = fromHallwayMoves Amber a1 occupied
        in  map (\(d, mp) -> (d, ms { a1Pos = mp })) moves
      else
        let moves = fromRoomMoves Amber a1 occupied
        in  map (\(d, mp) -> (d, ms { a1Pos = mp })) moves
    a2Moves :: [(Int, MazeState)]
    a2Moves = if Set.member a2 hallwayPositions
      then
        let moves = fromHallwayMoves Amber a2 occupied
        in  map (\(d, mp) -> (d, ms { a2Pos = mp })) moves
      else
        let moves = fromRoomMoves Amber a2 occupied
        in  map (\(d, mp) -> (d, ms { a2Pos = mp })) moves

    b1Moves :: [(Int, MazeState)]
    b1Moves = if Set.member b1 hallwayPositions
      then
        let moves = fromHallwayMoves Bronze b1 occupied
        in  map (\(d, mp) -> (d, ms { b1Pos = mp })) moves
      else
        let moves = fromRoomMoves Bronze b1 occupied
        in  map (\(d, mp) -> (d, ms { b1Pos = mp })) moves
    b2Moves :: [(Int, MazeState)]
    b2Moves = if Set.member b2 hallwayPositions
      then
        let moves = fromHallwayMoves Bronze b2 occupied
        in  map (\(d, mp) -> (d, ms { b2Pos = mp })) moves
      else
        let moves = fromRoomMoves Bronze b2 occupied
        in  map (\(d, mp) -> (d, ms { b2Pos = mp })) moves

    c1Moves :: [(Int, MazeState)]
    c1Moves = if Set.member c1 hallwayPositions
      then
        let moves = fromHallwayMoves Copper c1 occupied
        in  map (\(d, mp) -> (d, ms { c1Pos = mp })) moves
      else
        let moves = fromRoomMoves Copper c1 occupied
        in  map (\(d, mp) -> (d, ms { c1Pos = mp })) moves
    c2Moves :: [(Int, MazeState)]
    c2Moves = if Set.member c2 hallwayPositions
      then
        let moves = fromHallwayMoves Copper c2 occupied
        in  map (\(d, mp) -> (d, ms { c2Pos = mp })) moves
      else
        let moves = fromRoomMoves Copper c2 occupied
        in  map (\(d, mp) -> (d, ms { c2Pos = mp })) moves

    d1Moves :: [(Int, MazeState)]
    d1Moves = if Set.member d1 hallwayPositions
      then
        let moves = fromHallwayMoves Desert d1 occupied
        in  map (\(d, mp) -> (d, ms { d1Pos = mp })) moves
      else
        let moves = fromRoomMoves Desert d1 occupied
        in  map (\(d, mp) -> (d, ms { d1Pos = mp })) moves
    d2Moves :: [(Int, MazeState)]
    d2Moves = if Set.member d2 hallwayPositions
      then
        let moves = fromHallwayMoves Desert d2 occupied
        in  map (\(d, mp) -> (d, ms { d2Pos = mp })) moves
      else
        let moves = fromRoomMoves Desert d2 occupied
        in  map (\(d, mp) -> (d, ms { d2Pos = mp })) moves

-- Rooms to Hallways
-- Hallways to Rooms

fromRoomMoves :: AmphType -> MazePosition -> Set.Set MazePosition -> [(Int, MazePosition)]
fromRoomMoves aType mp occupied = undefined

fromHallwayMoves :: AmphType -> MazePosition -> Set.Set MazePosition -> [(Int, MazePosition)]
fromHallwayMoves aType mp occupied = case aType of
  Amber -> catMaybes [reach mp R11 occupied, reach mp R12 occupied]
  Bronze -> catMaybes [reach mp R21 occupied, reach mp R22 occupied]
  Copper -> catMaybes [reach mp R31 occupied, reach mp R32 occupied]
  Desert -> catMaybes [reach mp R41 occupied, reach mp R42 occupied]

reach :: MazePosition -> MazePosition -> Set.Set MazePosition -> Maybe (Int, MazePosition)
reach src dst occupied = undefined
  where
    blockerSquares = blockers M.! (src, dst)
    isBlocked = any (\mp -> Set.member mp occupied) blockerSquares

blockers :: M.Map (MazePosition, MazePosition) [MazePosition]
blockers = M.fromList
  [ ((H1, R12), [H2, R11])
  , ((H2, R12), [R11])
  , ((H4, R12), [R11])
  , ((H6, R12), [H4, R11])
  , ((H8, R12), [H6, H4, R11])
  , ((H10, R12), [H8, H6, H4, R11])
  , ((H11, R12), [H10, H8, H6, H4, R11])
  , ((H1, R11), [H2])
  , ((H2, R11), [])
  , ((H4, R11), [])
  , ((H6, R11), [H4])
  , ((H8, R11), [H6, H4])
  , ((H10, R11), [H8, H6, H4])
  , ((H11, R11), [H10, H8, H6, H4])
  , ((H1, R22), [H4, H2, R21])
  , ((H2, R22), [H4, R21])
  , ((H4, R22), [R21])
  , ((H6, R22), [R21])
  , ((H8, R22), [H6, R21])
  , ((H10, R22), [H8, H6, R21])
  , ((H11, R22), [H10, H8, H6, R21])
  , ((H1, R21), [H4, H2])
  , ((H2, R21), [H4])
  , ((H4, R21), [])
  , ((H6, R21), [])
  , ((H8, R21), [H6])
  , ((H10, R21), [H8, H6])
  , ((H11, R21), [H10, H8, H6])
  , ((H1, R32), [H6, H4, H2, R31])
  , ((H2, R32), [H6, H4, R31])
  , ((H4, R32), [H6, R31])
  , ((H6, R32), [R31])
  , ((H8, R32), [R31])
  , ((H10, R32), [H8, R31])
  , ((H11, R32), [H10, H8, R31])
  , ((H1, R31), [H6, H4, H2])
  , ((H2, R31), [H6, H4])
  , ((H4, R31), [H6])
  , ((H6, R31), [])
  , ((H8, R31), [])
  , ((H10, R31), [H8])
  , ((H11, R31), [H10, H8])
  , ((H1, R42), [H8, H6, H4, H2, R41])
  , ((H2, R42), [H8, H6, H4, R41])
  , ((H4, R42), [H8, H6, R41])
  , ((H6, R42), [H8, R41])
  , ((H8, R42), [R41])
  , ((H10, R42), [R41])
  , ((H11, R42), [H10, R41])
  , ((H1, R41), [H8, H6, H4, H2])
  , ((H2, R41), [H8, H6, H4])
  , ((H4, R41), [H8, H6])
  , ((H6, R41), [H8])
  , ((H8, R41), [])
  , ((H10, R41), [])
  , ((H11, R41), [H10])
  , ((R11, H1), [H2])
  , ((R11, H2), [])
  , ((R11, H4), [])
  , ((R11, H6), [H4])
  , ((R11, H8), [H6, H4])
  , ((R11, H10), [H8, H6, H4])
  , ((R11, H11), [H10, H8, H6, H4])
  , ((R12, H1), [H2, R11])
  , ((R12, H2), [R11])
  , ((R12, H4), [R11])
  , ((R12, H6), [H4, R11])
  , ((R12, H8), [H6, H4, R11])
  , ((R12, H10), [H8, H6, H4, R11])
  , ((R12, H11), [H10, H8, H6, H4, R11])
  , ((R21, H1), [H4, H2])
  , ((R21, H2), [H4])
  , ((R21, H4), [])
  , ((R21, H6), [])
  , ((R21, H8), [H6])
  , ((R21, H10), [H8, H6])
  , ((R21, H11), [H10, H8, H6])
  , ((R22, H1), [H4, H2, R21])
  , ((R22, H2), [H4, R21])
  , ((R22, H4), [R21])
  , ((R22, H6), [R21])
  , ((R22, H8), [H6, R21])
  , ((R22, H10), [H8, H6, R21])
  , ((R22, H11), [H10, H8, H6, R21])
  , ((R31, H1), [H6, H4, H2])
  , ((R31, H2), [H6, H4])
  , ((R31, H4), [H6])
  , ((R31, H6), [])
  , ((R31, H8), [])
  , ((R31, H10), [H8])
  , ((R31, H11), [H10, H8])
  , ((R32, H1), [H6, H4, H2, R31])
  , ((R32, H2), [H6, H4, R31])
  , ((R32, H4), [H6, R31])
  , ((R32, H6), [R31])
  , ((R32, H8), [R31])
  , ((R32, H10), [H8, R31])
  , ((R32, H11), [H10, H8, R31])
  , ((R41, H1), [H8, H6, H4, H2])
  , ((R41, H2), [H8, H6, H4])
  , ((R41, H4), [H8, H6])
  , ((R41, H6), [H8])
  , ((R41, H8), [])
  , ((R41, H10), [])
  , ((R41, H11), [H10])
  , ((R42, H1), [H8, H6, H4, H2, R41])
  , ((R42, H2), [H8, H6, H4, R41])
  , ((R42, H4), [H8, H6, R41])
  , ((R42, H6), [H8, R41])
  , ((R42, H8), [R41])
  , ((R42, H10), [R41])
  , ((R42, H11), [H10, R41])
  ]

mazeDistances :: M.Map (MazePosition, MazePosition) Int
mazeDistances = M.fromList
  [ ((H1, R12), 4)
  , ((H2, R12), 3)
  , ((H4, R12), 3)
  , ((H6, R12), 5)
  , ((H8, R12), 7)
  , ((H10, R12), 9)
  , ((H11, R12), 10)
  , ((H1, R11), 3)
  , ((H2, R11), 2)
  , ((H4, R11), 2)
  , ((H6, R11), 4)
  , ((H8, R11), 6)
  , ((H10, R11), 8)
  , ((H11, R11), 9)
  , ((H1, R22), 6)
  , ((H2, R22), 5)
  , ((H4, R22), 3)
  , ((H6, R22), 3)
  , ((H8, R22), 5)
  , ((H10, R22), 7)
  , ((H11, R22), 8)
  , ((H1, R21), 5)
  , ((H2, R21), 4)
  , ((H4, R21), 2)
  , ((H6, R21), 2)
  , ((H8, R21), 4)
  , ((H10, R21), 6)
  , ((H11, R21), 7)
  , ((H1, R32), 8)
  , ((H2, R32), 7)
  , ((H4, R32), 5)
  , ((H6, R32), 3)
  , ((H8, R32), 3)
  , ((H10, R32), 5)
  , ((H11, R32), 6)
  , ((H1, R31), 7)
  , ((H2, R31), 6)
  , ((H4, R31), 4)
  , ((H6, R31), 2)
  , ((H8, R31), 2)
  , ((H10, R31), 4)
  , ((H11, R31), 5)
  , ((H1, R42), 10)
  , ((H2, R42), 9)
  , ((H4, R42), 7)
  , ((H6, R42), 5)
  , ((H8, R42), 3)
  , ((H10, R42), 3)
  , ((H11, R42), 4)
  , ((H1, R41), 9)
  , ((H2, R41), 8)
  , ((H4, R41), 6)
  , ((H6, R41), 4)
  , ((H8, R41), 2)
  , ((H10, R41), 2)
  , ((H11, R41), 3)

  {-
  , ((R11, H1), [H2])
  , ((R11, H2), [])
  , ((R11, H4), [])
  , ((R11, H6), [H4])
  , ((R11, H8), [H6, H4])
  , ((R11, H10), [H8, H6, H4])
  , ((R11, H11), [H10, H8, H6, H4])
  , ((R12, H1), [H2, R11])
  , ((R12, H2), [R11])
  , ((R12, H4), [R11])
  , ((R12, H6), [H4, R11])
  , ((R12, H8), [H6, H4, R11])
  , ((R12, H10), [H8, H6, H4, R11])
  , ((R12, H11), [H10, H8, H6, H4, R11])
  , ((R21, H1), [H4, H2])
  , ((R21, H2), [H4])
  , ((R21, H4), [])
  , ((R21, H6), [])
  , ((R21, H8), [H6])
  , ((R21, H10), [H8, H6])
  , ((R21, H11), [H10, H8, H6])
  , ((R22, H1), [H4, H2, R21])
  , ((R22, H2), [H4, R21])
  , ((R22, H4), [R21])
  , ((R22, H6), [R21])
  , ((R22, H8), [H6, R21])
  , ((R22, H10), [H8, H6, R21])
  , ((R22, H11), [H10, H8, H6, R21])
  , ((R31, H1), [H6, H4, H2])
  , ((R31, H2), [H6, H4])
  , ((R31, H4), [H6])
  , ((R31, H6), [])
  , ((R31, H8), [])
  , ((R31, H10), [H8])
  , ((R31, H11), [H10, H8])
  , ((R32, H1), [H6, H4, H2, R31])
  , ((R32, H2), [H6, H4, R31])
  , ((R32, H4), [H6, R31])
  , ((R32, H6), [R31])
  , ((R32, H8), [R31])
  , ((R32, H10), [H8, R31])
  , ((R32, H11), [H10, H8, R31])
  , ((R41, H1), [H8, H6, H4, H2])
  , ((R41, H2), [H8, H6, H4])
  , ((R41, H4), [H8, H6])
  , ((R41, H6), [H8])
  , ((R41, H8), [])
  , ((R41, H10), [])
  , ((R41, H11), [H10])
  , ((R42, H1), [H8, H6, H4, H2, R41])
  , ((R42, H2), [H8, H6, H4, R41])
  , ((R42, H4), [H8, H6, R41])
  , ((R42, H6), [H8, R41])
  , ((R42, H8), [R41])
  , ((R42, H10), [R41])
  , ((R42, H11), [H10, R41])
  -}
  ]

hallwayPositions :: Set.Set MazePosition
hallwayPositions = Set.fromList [H1, H2, H4, H6, H8, H10, H11]









