module Second where

import Control.Monad
import Control.Monad.State
import qualified Data.Array as A
import Data.Char as A
import qualified Data.Foldable as F
import qualified Data.Heap as H
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Word
import System.Environment (getArgs)

import Lib (Coord, incrementKey, read2DDigits, getNeighbors, triangleNumber)

-- Day 12

solveCavePaths :: IO ()
solveCavePaths = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveCavePaths' fp

solveCavePaths' :: FilePath -> IO ()
solveCavePaths' fp = do
  caveGraph <- parseCaveGraph fp
  print $ countPaths1 caveGraph
  print $ countPaths caveGraph

type CaveGraph = M.Map String (Set.Set String)

isBigCave :: String -> Bool
isBigCave = all isUpper

parseCaveGraph :: FilePath -> IO CaveGraph
parseCaveGraph fp = do
  edges <- (map parseSingleLine . lines) <$> readFile fp
  let emptyGraph = foldl addNodesFold M.empty edges
  return $ foldl addEdgesFold emptyGraph edges
  where
    parseSingleLine :: String -> (String, String)
    parseSingleLine ln = case splitOn "-" ln of
      [n1, n2] -> (n1, n2)
      _ -> error $ "Could not parse graph line: " ++ ln

    addNodesFold :: CaveGraph -> (String, String) -> CaveGraph
    addNodesFold gr1 (n1, n2) = M.insert n2 Set.empty $
      (M.insert n1 (Set.empty) gr1)

    addEdgesFold :: CaveGraph -> (String, String) -> CaveGraph
    addEdgesFold gr1 (n1, n2) =
      let n1Edges = gr1 M.! n1
          n2Edges = gr1 M.! n2
          gr2 = M.insert n1 (Set.insert n2 n1Edges) gr1
      in  M.insert n2 (Set.insert n1 n2Edges) gr2

countPaths1 :: CaveGraph -> Int
countPaths1 caveGraph = countPathsFromState caveGraph
  (["start"], Set.singleton "start", True)

countPaths :: CaveGraph -> Int
countPaths caveGraph = countPathsFromState caveGraph
  (["start"], Set.singleton "start", False)

-- Bool is "have we used our second small cave visit"
type CGSState = ([String], Set.Set String, Bool)

countPathsFromState :: CaveGraph -> CGSState -> Int
countPathsFromState _ ([], _, _) = 0
countPathsFromState caveGraph cgs@(currentPath, visitedSmallCaves, usedSecond) =
  if head currentPath == "end"
    then 1
    else sum $ map calculateMove (Set.toList nextMoves)
  where
    nextMoves = getLegalMoves caveGraph cgs

    calculateMove :: String -> Int
    calculateMove newMove = countPathsFromState caveGraph
      ( newMove : currentPath
      , newVisitedForMove newMove
      , usedSecond || newUsedForMove newMove
      )

    newVisitedForMove :: String -> Set.Set String
    newVisitedForMove newMove = if isBigCave newMove then visitedSmallCaves
      else Set.insert newMove visitedSmallCaves

    newUsedForMove :: String -> Bool
    newUsedForMove newMove = Set.member newMove visitedSmallCaves

getLegalMoves :: CaveGraph -> CGSState -> Set.Set String
getLegalMoves _ ([], _, _) = Set.empty
getLegalMoves caveGraph ((currentNode : _), visited, usedSecond) =
  case M.lookup currentNode caveGraph of
    Nothing -> Set.empty
    Just nodes ->
      let nodes' = Set.delete "start" nodes
      in if usedSecond then Set.difference nodes' visited else nodes'

-- Day 13

solveOrigami :: IO ()
solveOrigami = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveOrigami' fp

solveOrigami' :: FilePath -> IO ()
solveOrigami' fp = do
  r@(dots, folds) <- parseOrigamiPatterns fp
  let numColumns = maximum (fst <$> dots) + 1
  let numRows = maximum (snd <$> dots) + 1
  let foldState0 = FoldState (Set.fromList dots) numRows numColumns
  -- let afterFirst = handleFold (head folds) foldState0
  -- print $ Set.size (fsDots afterFirst)
  let finalConfig = foldl (\fs fld -> handleFold fld fs) foldState0 folds
  let file = "inputs/day_13_output.txt"
  writeFile file (stringForFoldState finalConfig)
  

data OFold = XFold Int | YFold Int
  deriving (Show)

data FoldState = FoldState
  { fsDots :: Set.Set Coord
  , numRows :: Int
  , numCols :: Int
  }

parseOrigamiPatterns :: FilePath -> IO ([Coord], [OFold])
parseOrigamiPatterns fp = do
  allLines <- lines <$> readFile fp
  let (coordLines, remainingLines) = span (\s -> ',' `elem` s) allLines
  return
    ( parse2DCoord <$> coordLines
    , if null remainingLines then [] else parseFold <$> tail remainingLines
    )
  where
    parseFold :: String -> OFold
    parseFold foldLine = case splitOn "=" foldLine of
      [p1, number] -> if last p1 == 'x'
        then XFold (read number) else YFold (read number)
      _ -> error $ "Invalid fold line " ++ foldLine

parse2DCoord :: String -> Coord
parse2DCoord s = case splitOn "," s of
  [x, y] -> (read x, read y)
  _ -> error $ "Can't parse 2D coord: " ++ s

handleFold :: OFold -> FoldState -> FoldState
handleFold (YFold axis) (FoldState dots numRows numCols) = FoldState
  (Set.map shiftFunction dots) axis numCols
  where
    shiftFunction (c, r) = if r < axis then (c, r)
      else (c, axis - (r - axis))
handleFold (XFold axis) (FoldState dots numRows numCols) = FoldState
  (Set.map shiftFunction dots) numRows axis
  where
    shiftFunction (c, r) = if c < axis then (c, r)
      else (axis - (c - axis), r)


stringForFoldState :: FoldState -> String
stringForFoldState (FoldState dots numRows numCols) = unlines $
  map rowFunction [0..(numRows)]
  where
    rowFunction :: Int -> String
    rowFunction r = map (charFunction r) [0..(numCols)]

    charFunction :: Int -> Int -> Char
    charFunction r c = if Set.member (c, r) dots then '#' else ' '

-- Day 14

solvePolymer :: IO ()
solvePolymer = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solvePolymer' fp

solvePolymer' :: FilePath -> IO ()
solvePolymer' fp = do
  (template, insertionMap) <- parsePolymer fp
  -- let finalString = foldl (expandPolymer insertionMap) template [1..10]
  -- let numOccurrences = findNumOccurrences finalString
  -- let numbers = snd <$> (M.toList numOccurrences)
  -- print $ maximum numbers - minimum numbers
  let initialPolymerMap = buildTemplateMap template
  let finalMap = foldl (expandPolymerLong insertionMap) initialPolymerMap [1..40]
  let individualCounts = gatherCounts finalMap
  let adjustedCounts = adjustCounts template individualCounts
  print $ maximum (snd <$> adjustedCounts) - minimum (snd <$> adjustedCounts)

type InsertionMap = M.Map (Char, Char) Char

parsePolymer :: FilePath -> IO (String, InsertionMap)
parsePolymer fp = do
  inputLines <- lines <$> readFile fp
  when (length inputLines < 3) (error "Invalid input")
  let (pattern : _ : insertionRules) = inputLines
  let rules = parseInsertionRule <$> insertionRules
  let finalMap = foldl insertFold M.empty rules
  return (pattern, finalMap)
  where
    insertFold :: InsertionMap -> (Char, Char, Char) -> InsertionMap
    insertFold prevMap (a, b, c) = M.insert (a, b) c prevMap

parseInsertionRule :: String -> (Char, Char, Char)
parseInsertionRule input = case splitOn " -> " input of
  [pair, newChar] -> if length pair /= 2 && length newChar /= 1
    then error $ "Invalid insertion rule: " ++ input
    else (head pair, pair !! 1, head newChar)
  _ -> error $ "Invalid insertion rule: " ++ input

expandPolymer :: InsertionMap -> String -> Int -> String
expandPolymer insertionMap startingPolymer _ = expandTail "" startingPolymer
  where
    expandTail :: String -> String -> String
    expandTail accum [] = reverse accum
    expandTail accum [lastChar] = reverse (lastChar : accum)
    expandTail accum (firstChar : secondChar : rest) =
      let newChar = insertionMap M.! (firstChar, secondChar)
      in  expandTail (newChar : firstChar : accum) (secondChar : rest)

findNumOccurrences :: String -> M.Map Char Int
findNumOccurrences = foldl foldFNO M.empty
  where
    foldFNO :: M.Map Char Int -> Char -> M.Map Char Int
    foldFNO prevMap c = incrementKey c prevMap

type PolymerPairMap = M.Map (Char, Char) Integer

expandPolymerLong :: InsertionMap -> PolymerPairMap -> Int -> PolymerPairMap
expandPolymerLong insertionMap originalMap _ =
  foldl expandTail M.empty (M.toList originalMap)
  where
  expandTail :: PolymerPairMap -> ((Char, Char), Integer) -> PolymerPairMap
  expandTail accumMap ((newA, newB), count) =
    let newChar = insertionMap M.! (newA, newB)
        newPair1 = (newA, newChar)
        newPair2 = (newChar, newB)
    in  addForKey newPair2 count (addForKey newPair1 count accumMap)

addForKey :: (Ord k, Integral i) => k -> i -> M.Map k i -> M.Map k i
addForKey key count prevMap = case M.lookup key prevMap of
  Nothing -> M.insert key count prevMap
  Just j -> M.insert key (j + count) prevMap

buildTemplateMap :: String -> PolymerPairMap
buildTemplateMap = buildTail M.empty
  where
    buildTail :: PolymerPairMap -> String -> PolymerPairMap
    buildTail prevMap [] = prevMap
    buildTail prevMap [a] = prevMap
    buildTail prevMap (a : b : rest) =
      buildTail (addForKey (a, b) 1 prevMap) (b : rest)

gatherCounts :: PolymerPairMap -> M.Map Char Integer
gatherCounts pairMap = foldl f M.empty (M.toList pairMap)
  where
    f :: M.Map Char Integer -> ((Char, Char), Integer) -> M.Map Char Integer
    f prevMap ((a, b), count) = addForKey b count (addForKey a count prevMap)

adjustCounts :: String -> M.Map Char Integer -> [(Char, Integer)]
adjustCounts input charMap = foldl adjustTail [] (M.toList charMap)
  where
    firstChar = head input
    lastChar = last input

    adjustTail :: [(Char, Integer)] -> (Char, Integer) -> [(Char, Integer)]
    adjustTail prevMap (a, count) = if a == firstChar || a == lastChar
      then (a, ((count - 1) `quot` 2) + 1) : prevMap
      else (a, count `quot` 2) : prevMap

-- Day 15
solveChiton :: IO ()
solveChiton = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveChiton' fp

solveChiton' :: FilePath -> IO ()
solveChiton' fp = do
  riskMap <- read2DDigits fp
  let augmentedRiskMap = augmentRisks riskMap
  let (dist, prev) = dijkstra2d augmentedRiskMap (0, 0) (snd $ A.bounds augmentedRiskMap)
  -- print augmentedRiskMap
  print $ snd $ A.bounds augmentedRiskMap
  print $ dist M.! (snd $ A.bounds augmentedRiskMap)
  -- print $ dijkstra2d riskMap dist1 uv0 heap0 (0, 0) (snd $ A.bounds riskMap)

augmentRisks :: DistanceArray -> DistanceArray
augmentRisks riskMap = A.array ((0, 0), maximum (fst <$> finalAssocs)) finalAssocs
  where
    (rows, cols) = snd $ A.bounds riskMap

    expandedTile :: Int -> Int -> (Coord, Int) -> [(Coord, Int)]
    expandedTile shiftRow shiftCol ((r0, c0), x) =
      [ ((r0, c0), x)
      , ((r0, c0 + shiftCol), incMod10 x 1)
      , ((r0, c0 + (shiftCol * 2)), incMod10 x 2)
      , ((r0, c0 + (shiftCol * 3)), incMod10 x 3)
      , ((r0, c0 + (shiftCol * 4)), incMod10 x 4)
      , ((r0 + shiftRow, c0), incMod10 x 1)
      , ((r0 + shiftRow, c0 + shiftCol), incMod10 x 2)
      , ((r0 + shiftRow, c0 + (shiftCol * 2)), incMod10 x 3)
      , ((r0 + shiftRow, c0 + (shiftCol * 3)), incMod10 x 4)
      , ((r0 + shiftRow, c0 + (shiftCol * 4)), incMod10 x 5)
      , ((r0 + (shiftRow * 2), c0), incMod10 x 2)
      , ((r0 + (shiftRow * 2), c0 + shiftCol), incMod10 x 3)
      , ((r0 + (shiftRow * 2), c0 + (shiftCol * 2)), incMod10 x 4)
      , ((r0 + (shiftRow * 2), c0 + (shiftCol * 3)), incMod10 x 5)
      , ((r0 + (shiftRow * 2), c0 + (shiftCol * 4)), incMod10 x 6)
      , ((r0 + (shiftRow * 3), c0), incMod10 x 3)
      , ((r0 + (shiftRow * 3), c0 + shiftCol), incMod10 x 4)
      , ((r0 + (shiftRow * 3), c0 + (shiftCol * 2)), incMod10 x 5)
      , ((r0 + (shiftRow * 3), c0 + (shiftCol * 3)), incMod10 x 6)
      , ((r0 + (shiftRow * 3), c0 + (shiftCol * 4)), incMod10 x 7)
      , ((r0 + (shiftRow * 4), c0), incMod10 x 4)
      , ((r0 + (shiftRow * 4), c0 + shiftCol), incMod10 x 5)
      , ((r0 + (shiftRow * 4), c0 + (shiftCol * 2)), incMod10 x 6)
      , ((r0 + (shiftRow * 4), c0 + (shiftCol * 3)), incMod10 x 7)
      , ((r0 + (shiftRow * 4), c0 + (shiftCol * 4)), incMod10 x 8)
      ]

    finalAssocs :: [(Coord, Int)]
    finalAssocs = concatMap (expandedTile (rows + 1) (cols + 1)) (A.assocs riskMap)

    incMod10 :: Int -> Int -> Int
    incMod10 x increment =
      let r1 = x + increment
      in  if r1 >= 10 then r1 - 9 else r1

-- Heap/Priority Queue of Things
type HeapCell = (Int, Coord)
type ChitonHeap = H.MinHeap HeapCell
type DistanceArray = A.Array Coord Int
type DistanceMap = M.Map Coord Int
type CoordMap = A.Array Coord (Maybe Coord)
type CoordSet = Set.Set Coord

data DijkstraState = DijkstraState
  { djDist :: DistanceMap
  , djPrev :: CoordMap
  , djQueue :: ChitonHeap
  , djUnvisited :: CoordSet
  }

dijkstra2d :: DistanceArray -> Coord -> Coord -> (DistanceMap, CoordMap)
dijkstra2d edgeDistances start dst = dijkstraTail (DijkstraState d0 p0 q0 v0)
  where
    d0 = M.insert start 0 $
      M.fromList (zip (A.indices edgeDistances) (repeat (maxBound :: Int)))
    p0 = A.listArray (A.bounds edgeDistances) (repeat Nothing)
    q0 = H.fromList (map (\(c, d) -> (d, c)) (M.toList d0))
    v0 = (Set.fromList (A.indices edgeDistances))

    dijkstraTail :: DijkstraState -> (DistanceMap, CoordMap)
    dijkstraTail djs@(DijkstraState d1 p1 q1 v1) = case H.view q1 of
      Nothing -> (d1, p1)
      -- Check right away if coordU is in Unvisited?
      Just ((distU, u), restHeap) -> if Set.member u v1
        then
          let v2 = Set.delete u v1
              neighbors = getNeighbors (snd $ A.bounds edgeDistances) u 
              newNeighbors = filter (\n -> Set.member n v1) neighbors
              djs2 = foldl (dijkstraFold u) (djs{djUnvisited = v2}) newNeighbors
          in  if u == dst then (djDist djs2, djPrev djs2)
                else dijkstraTail djs2
        else dijkstraTail (DijkstraState d1 p1 restHeap v1)

    dijkstraFold :: Coord -> DijkstraState -> Coord -> DijkstraState
    dijkstraFold u djs@(DijkstraState d1 p1 q1 v1) v =
      let d1U = d1 M.! u
          alt = if d1U == maxBound then maxBound else d1U + edgeDistances A.! v
          d2 = M.insert v alt d1
          p2 = p1 -- A.// [(v, Just u)]
          q2 = H.insert (alt, v) q1
      in  if alt < d1 M.! v
            then DijkstraState d2 p2 q2 v1
            else djs


-- Day 16

solveBinary :: IO ()
solveBinary = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveBinary' fp

solveBinary' :: FilePath -> IO ()
solveBinary' fp = do
  binaryLines <- lines <$> readFile fp
  let trees = map parsePacketHierarchy binaryLines
  let results = map valueForPacket trees
  print results

literalPacketType :: Word8
literalPacketType = 4

data PacketNode =
  Literal Word8 Word64 |
  Operator Word8 Word8 [PacketNode]
  deriving (Show)

data Bit = Zero | One
  deriving (Show, Eq)

boolToBit :: Bool -> Bit
boolToBit False = Zero
boolToBit True = One

sumPacketVersions :: PacketNode -> Word64
sumPacketVersions (Literal v _ ) = fromIntegral v
sumPacketVersions (Operator v _ otherNodes) =
  fromIntegral v + sum (sumPacketVersions <$> otherNodes)

valueForPacket :: PacketNode -> Word64
valueForPacket (Literal _ value) = value
valueForPacket (Operator _ 0 packets) = sum (valueForPacket <$> packets)
valueForPacket (Operator _ 1 packets) = product (valueForPacket <$> packets)
valueForPacket (Operator _ 2 packets) = minimum (valueForPacket <$> packets)
valueForPacket (Operator _ 3 packets) = maximum (valueForPacket <$> packets)
valueForPacket (Operator _ 5 [p1, p2]) =
  if valueForPacket p1 > valueForPacket p2 then 1 else 0
valueForPacket (Operator _ 6 [p1, p2]) =
  if valueForPacket p1 < valueForPacket p2 then 1 else 0
valueForPacket (Operator _ 7 [p1, p2]) =
  if valueForPacket p1 == valueForPacket p2 then 1 else 0

-- "D2FE28" -> Literal 6 2021
parsePacketHierarchy :: String -> PacketNode
parsePacketHierarchy input = fst (parsePacketBits allBits)
  where
    allBits :: [Bit]
    allBits = concatMap parseHexChar input

parsePacketBits :: [Bit] -> (PacketNode, [Bit])
parsePacketBits bits = if length bits < 6
  then error "Not enough bits!" -- < Is this right?
  else if packetType == literalPacketType
    then parseLiteralPacket version rest
    else parseOperatorPacket version packetType rest
  where
    (b1 : b2 : b3 : b4 : b5 : b6 : rest) = bits
    version = bitsToDecimal8 [b1, b2, b3]
    packetType = bitsToDecimal8 [b4, b5, b6]

parseLiteralPacket :: Word8 -> [Bit] -> (PacketNode, [Bit])
parseLiteralPacket version bits = (Literal version resultNum, remainder)
  where
    (resultNum, remainder) = plpTail [] bits
    plpTail :: [Bit] -> [Bit] -> (Word64, [Bit])
    plpTail accum remainder = if length remainder < 5
      then (bitsToDecimal64 accum, remainder)
      else
        let (newBits, rest) = splitAt 4 (tail remainder)
        in  if head remainder == Zero
              then (bitsToDecimal64 (accum ++ newBits), rest)
              else plpTail (accum ++ newBits) rest

parseOperatorPacket :: Word8 -> Word8 -> [Bit] -> (PacketNode, [Bit])
parseOperatorPacket version oType bits = if null bits
  then error "No bits for operator!"
  else (Operator version oType resultPackets, remainder)
  where
    (resultPackets, remainder) = case head bits of
      Zero -> parseOperatorBitsLength (tail bits)
      One -> parseOperatorNumPackets (tail bits)

parseOperatorBitsLength :: [Bit] -> ([PacketNode], [Bit])
parseOperatorBitsLength bits = if length bits < 15
  then error "Not enough to parse bit length of operator"
  else (packets, drop (fromIntegral (15 + bitLength)) bits)
  where
    bitLength = bitsToDecimal64 (take 15 bits)
    packets = poblTail [] (take (fromIntegral bitLength) (drop 15 bits))

    poblTail :: [PacketNode] -> [Bit] -> [PacketNode]
    poblTail accum [] = reverse accum
    poblTail accum bits' =
      let (newNode, remainder) = parsePacketBits bits'
      in  poblTail (newNode : accum) remainder

parseOperatorNumPackets :: [Bit] -> ([PacketNode], [Bit])
parseOperatorNumPackets bits = if length bits < 11
  then error "Not enough to parse number of operator packets"
  else ponpTail [] (drop 11 bits) (fromIntegral numPackets)
  where
    numPackets = bitsToDecimal64 (take 11 bits)

    ponpTail :: [PacketNode] -> [Bit] -> Int -> ([PacketNode], [Bit])
    ponpTail accum bits 0 = (reverse accum, bits)
    ponpTail accum bits n =
      let (newPacketNode, remainder) = parsePacketBits bits
      in  ponpTail (newPacketNode : accum) remainder (n - 1)

-- [One, Zero, Zero, One] -> 9
-- [Zero, One, Zero, Zero, One] -> 9
-- [One, Zero, One, Zero, One, One] -> 43
bitsToDecimal8 :: [Bit] -> Word8
bitsToDecimal8 bits = if length bits > 8
  then error $ "Too long! Use bitsToDecimal64! " ++ show bits
  else btd8 0 1 (reverse bits)
    where
      btd8 :: Word8 -> Word8 -> [Bit] -> Word8
      btd8 accum _ [] = accum
      btd8 accum mult (b : rest) = case b of
        Zero -> btd8 accum (mult * 2) rest
        One -> btd8 (accum + mult) (mult * 2) rest

-- [One, Zero, Zero, One] -> 9
-- [Zero, One, Zero, Zero, One] -> 9
-- [One, Zero, One, Zero, One, One] -> 43
bitsToDecimal64 :: [Bit] -> Word64
bitsToDecimal64 bits = if length bits > 64
  then error $ "Too long! Use Integer or something! " ++ show bits
  else btd64 0 1 (reverse bits)
    where
      btd64 :: Word64 -> Word64 -> [Bit] -> Word64
      btd64 accum _ [] = accum
      btd64 accum mult (b : rest) = case b of
        Zero -> btd64 accum (mult * 2) rest
        One -> btd64 (accum + mult) (mult * 2) rest

parseHexChar :: Char -> [Bit]
parseHexChar '0' = [Zero, Zero, Zero, Zero]
parseHexChar '1' = [Zero, Zero, Zero, One]
parseHexChar '2' = [Zero, Zero, One, Zero]
parseHexChar '3' = [Zero, Zero, One, One]
parseHexChar '4' = [Zero, One, Zero, Zero]
parseHexChar '5' = [Zero, One, Zero, One]
parseHexChar '6' = [Zero, One, One, Zero]
parseHexChar '7' = [Zero, One, One, One]
parseHexChar '8' = [One, Zero, Zero, Zero]
parseHexChar '9' = [One, Zero, Zero, One]
parseHexChar 'A' = [One, Zero, One, Zero]
parseHexChar 'B' = [One, Zero, One, One]
parseHexChar 'C' = [One, One, Zero, Zero]
parseHexChar 'D' = [One, One, Zero, One]
parseHexChar 'E' = [One, One, One, Zero]
parseHexChar 'F' = [One, One, One, One]
parseHexChar c = error $ "Invalid Hex Char: " ++ [c]

-- Day 17

solveProbe :: IO ()
solveProbe = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveProbe' fp

solveProbe' :: FilePath -> IO ()
solveProbe' fp = do
  targetRegion <- parseTargetRegion fp
  let wins = passingTrajectories targetRegion
  let bestY = maximumBy (\v1 v2 -> compare (vY v1) (vY v2)) wins
  print $ triangleNumber (vY bestY)
  print $ length wins

data TargetRegion = TargetRegion
  { minX :: Int
  , maxX :: Int
  , minY :: Int
  , maxY :: Int
  } deriving (Show)

data Velocities = Velocities { vX :: Int, vY :: Int}
  deriving (Show)

-- Parse Target Dimensions
parseTargetRegion :: FilePath -> IO TargetRegion
parseTargetRegion fp = do
  fileLines <- lines <$> readFile fp
  let str0 = head fileLines
  let (x0, str1) = span (/= '.') (drop 15 str0)
  let (x1, str2) = span (/= ',') (dropWhile (== '.') str1)
  let (y0, str3) = span (/= '.') (drop 4 str2)
  let y1 = dropWhile (== '.') str3
  let [x0', x1', y0', y1'] = read <$> [x0, x1, y0, y1]
  let (xMin, xMax, yMin, yMax) = ( min x0' x1'
                                 , max x0' x1'
                                 , min y0' y1'
                                 , max y0' y1')
  return $ TargetRegion xMin xMax yMin yMax


-- Given Velocities, Does it reach target?
hitsTarget :: Velocities -> TargetRegion -> Bool
hitsTarget v1@(Velocities vx vy) (TargetRegion x0 x1 y0 y1) =
  hitsTarget' (0, 0) v1
  where
  hitsTarget' :: Coord -> Velocities -> Bool
  hitsTarget' s@(x, y) (Velocities vx' vy') = if withinTarget s
    then True
    else if pastTarget s then False
      else
        let newXPos = x + vx'
            newYPos = y + vy'
            newVx = max (vx' - 1) 0
            newVy = vy' - 1
        in  hitsTarget' (newXPos, newYPos) (Velocities newVx newVy)

  withinTarget :: Coord -> Bool
  withinTarget (x, y) = x >= x0 && x <= x1 && y >= y0 && y <= y1
  
  pastTarget :: Coord -> Bool
  pastTarget (x, y) = x > x1 || y < y0

-- Determine the range of velocities we want to search
-- How do we best search that region

-- Lowest Y (below target region)
-- Highest Y
-- Lowest X (triangle number < left end of region)
-- Highest X (furthest end of the target region)

xVelocityRange :: TargetRegion -> (Int, Int)
xVelocityRange (TargetRegion x0 x1 _ _) = case lowestX of
  Just x' -> (x', x1 + 1)
  Nothing -> error "Can't find x velocity range!"
  where
    lowestX = find (\x -> triangleNumber x >= x0) [1..x1]

yVelocityRange :: TargetRegion -> (Int, Int)
yVelocityRange (TargetRegion _ _ y0 _) = (min y0 (-y0), max y0 (-y0))

totalRange :: TargetRegion -> [Velocities]
totalRange tr = [(Velocities x y) | x <- [vx0..vx1], y <- [vy0..vy1]]
  where
    (vx0, vx1) = xVelocityRange tr
    (vy0, vy1) = yVelocityRange tr

passingTrajectories :: TargetRegion -> [Velocities]
passingTrajectories tr = filter (\v -> hitsTarget v tr) (totalRange tr)

-- Day 18
solveSnailNumbers :: IO ()
solveSnailNumbers = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveSnailNumbers' fp

solveSnailNumbers' :: FilePath -> IO ()
solveSnailNumbers' fp = do
  all@(firstNum : restNums) <- parseInputNumbers fp
  -- let finalNum = foldl addNumbers firstNum restNums
  -- print $ finalNum
  -- print $ findMagnitude finalNum
  let pairs = getAllPairs all
  let scores = map (\(x, y) -> findMagnitude (addNumbers x y)) pairs
  print $ maximum scores

getAllPairs :: [a] -> [(a, a)]
getAllPairs items = [(x, y) | (i1, x) <- iwn, (i2, y) <- iwn, i1 /= i2]
  where
   iwn = zip [1..] items

data NumberNode =
  Single Int |
  Branch NumberNode NumberNode
  deriving (Show)

parseInputNumbers :: FilePath -> IO [NumberNode]
parseInputNumbers fp = (map parseNumberNode . lines) <$> readFile fp

parseNumberNode :: String -> NumberNode
parseNumberNode input = fst (parseGenericNumber input) -- < Assume rest is ""
  where
    -- Case where it's a literal
    parseGenericNumber :: String -> (NumberNode, String)
    parseGenericNumber input = if isNumber (head input)
      then parseSingleNumber input
      else if head input /= '['
        then error $ "Invalid input: " ++ input
        else
          -- rest0 ought to begin with a comma (assert this?)
          let (firstNumber, rest0) = parseGenericNumber (tail input)
          -- rest1 ought to begin with a right bracket (assert this?)
              (secondNumber, rest1) = parseGenericNumber (tail rest0)
          in  (Branch firstNumber secondNumber, tail rest1)

    parseSingleNumber :: String -> (NumberNode, String)
    parseSingleNumber input =
      let (numLiteral, rest) = span isNumber input
      in  (Single (read numLiteral), rest)

-- -> [[[[0,7],4],[[7,8],[6,0]]],[8,1]
addNumbers :: NumberNode -> NumberNode -> NumberNode
addNumbers n1 n2 = addReduce initialPoint
  where
    initialPoint = Branch n1 n2

    addReduce :: NumberNode -> NumberNode
    addReduce n0 = case reduceSnailNumberE n0 of
      (n1, False) -> case reduceSnailNumberSplit n1 of
        (n2, False) -> n2
        (n2, True) -> addReduce n2
      (n1, True) -> addReduce n1

-- reduceTail  -> [[[[0,9],2],3,4]
a :: NumberNode
a = parseNumberNode "[[[[[9,8],1],2],3],4]"

-- reduceTail  -> [7,[6,[5,[7,0]]]]
b :: NumberNode
b = parseNumberNode "[7,[6,[5,[4,[3,2]]]]]"

-- reduceTail -> [[6,[5,[7,0]]],3]
c :: NumberNode
c = parseNumberNode "[[6,[5,[4,[3,2]]]],1]"

d :: NumberNode
d = parseNumberNode "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"

e :: NumberNode
e = parseNumberNode "[[[[4,3],4],4],[7,[[8,4],9]]]"

f :: NumberNode
f = parseNumberNode "[1,1]"

-- Branch (Branch (Branch (Branch (Branch (Single 9) (Single 8)) (Single 1)) (Single 2) ) (Single 3)) (Single 4)

-- Branch (Branch (Single 6) (Branch (Single 5) (Branch (Single 4) (Branch (Single 3) (Single 2))))) (Single 1)

data Resolution = PassRight Int | PassLeft Int | NoIssue | ResolvedIssue

reduceSnailNumberE :: NumberNode -> (NumberNode, Bool)
reduceSnailNumberE (Single i) = (Single i, False)
reduceSnailNumberE (Branch leftBranch rightBranch) =
  case reduceTail leftBranch 1 of
    (rl, PassLeft i) -> (Branch rl rightBranch, True)
    (rl, PassRight i) -> (Branch rl (addLeft i rightBranch), True)
    (rl, ResolvedIssue) -> (Branch rl rightBranch, True)
    (rl, NoIssue) -> case reduceTail rightBranch 1 of
      (rr, PassLeft i) -> (Branch (addRight i rl) rr, True)
      (rr, PassRight i) -> (Branch rl rr, True)
      (rr, ResolvedIssue) -> (Branch rl rr, True)
      (rr, NoIssue) -> (Branch rl rr, False)
  where
    reduceTail :: NumberNode -> Int -> (NumberNode, Resolution)
    reduceTail (Single j) _ = (Single j, NoIssue)
    reduceTail (Branch leftBranch' rightBranch') n = if n < 3
      then case reduceTail leftBranch' (n + 1) of
        (rL, ResolvedIssue) -> (Branch rL rightBranch', ResolvedIssue)
        (rL, PassLeft i) -> (Branch rL rightBranch', PassLeft i)
        (rL, PassRight i) -> (Branch rL (addLeft i rightBranch'), ResolvedIssue)
        (rL, NoIssue) -> case reduceTail rightBranch' (n + 1) of
          (rR, ResolvedIssue) -> (Branch rL rR, ResolvedIssue)
          (rR, PassLeft j) -> (Branch (addRight j rL) rR, ResolvedIssue)
          (rR, PassRight j) -> (Branch rL rR, PassRight j)
          (rR, NoIssue) -> (Branch rL rR, NoIssue)
      else case (leftBranch', rightBranch') of
        (Single x, Single y) -> (Branch leftBranch' rightBranch', NoIssue)
        (Single x, Branch (Single y) (Single z)) ->
          (Branch (Single (x + y)) (Single 0), PassRight z)
        (Branch (Single x) (Single y), rb) ->
          (Branch (Single 0) (addLeft y rb), PassLeft x)
        _ -> error "Invalid case"

reduceSnailNumberSplit :: NumberNode -> (NumberNode, Bool)
reduceSnailNumberSplit (Single i) = if i >= 10
  then
    let half = i `quot` 2
        isOdd = i `mod` 2 == 1
    in  (Branch (Single half) (Single (if isOdd then half + 1 else half)), True)
  else (Single i, False)
reduceSnailNumberSplit (Branch lb rb) = case reduceSnailNumberSplit lb of
  (rL, True) -> (Branch rL rb, True)
  (rL, False) ->
    let (rR, resolution) = reduceSnailNumberSplit rb
    in  (Branch rL rR, resolution)

addLeft :: Int -> NumberNode -> NumberNode
addLeft x (Single y) = Single (x + y)
addLeft x (Branch lb rb) = (Branch (addLeft x lb) rb)

addRight :: Int -> NumberNode -> NumberNode
addRight x (Single y) = Single (x + y)
addRight x (Branch lb rb) = Branch lb (addRight x rb)

findMagnitude :: NumberNode -> Int
findMagnitude (Single i) = i
findMagnitude (Branch lb rb) = 3 * (findMagnitude lb) + 2 * (findMagnitude rb)

-- Day 19

solveSensors :: IO ()
solveSensors = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveSensors' fp

solveSensors' :: FilePath -> IO ()
solveSensors' fp = do
  sensorMap <- parseSensorReadings fp
  let allKeys = M.keys sensorMap
  -- Get every pair
  let allPairs = [(i, j, sensorMatches (i, sensorMap M.! i) (j, sensorMap M.! j)) | i <- allKeys, j <- allKeys, i /= j]
  let filteredPairs = filter (\(i, j, result) -> isJust result) allPairs
  let results = searchPairs (map (\(i, j, r) -> (i, j, fromJust r)) filteredPairs)
  -- let finalSet = foldl (foldSensorPositions sensorMap) Set.empty results
  -- print $ Set.size finalSet
  let locations = map findFinalLocation results
  let distances = [manhattan3 l1 l2 | (i, l1) <- locations, (j, l2) <- locations, i < j]
  print $ maximum distances

manhattan3 :: Coord3 -> Coord3 -> Int
manhattan3 (x1, y1, z1) (x2, y2, z2) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

type SensorReadingMap = M.Map Int [Coord3]

parseSensorReadings :: FilePath -> IO SensorReadingMap
parseSensorReadings fp = do
  inputLines <- lines <$> readFile fp
  return $ parseReadings [] inputLines
  where
    parseReadings :: [(Int, [Coord3])] -> [String] -> SensorReadingMap
    parseReadings accum [] = M.fromList accum
    parseReadings accum inputs =
      let ((sensorLine1 : readings), restInputs) = span (/= "") inputs
          newTuple = (parseSensorNumber sensorLine1, parseReading <$> readings)
          newTail = if null restInputs then restInputs
                      else tail restInputs
      in  parseReadings (newTuple : accum) newTail
    
    parseSensorNumber :: String -> Int
    parseSensorNumber inputLine =  read $ takeWhile isNumber (drop 12 inputLine)

    parseReading :: String -> Coord3
    parseReading reading = case splitOn "," reading of
      [r1, r2, r3] -> (read r1, read r2, read r3)
      _ -> error $ "Bad reading: " ++ reading

-- If we find a configuration with at least 12 matches...we want to return the offset position of the sensor.
sensorMatches :: (Int, [Coord3]) -> (Int, [Coord3]) -> Maybe Transform
sensorMatches (_, firstReadings) (_, secondReadings) = makeFinalTransform <$> firstGoodSet
  where
    firstRealignments :: [(Coord3, Set.Set Coord3)]
    firstRealignments = map (\p -> (p, realignSensors p firstReadings)) firstReadings

    secondRealignments :: [(Coord3, Set.Set Coord3)]
    secondRealignments = map (\p -> (p, realignSensors p secondReadings)) secondReadings

    transformedRealignments :: [(Transform, Set.Set Coord3)]
    transformedRealignments = [makeTransformWithSet o tf ra | tf <- tfs, (o, ra) <- secondRealignments]

    tfsWithCount =
      [(o1, tf, Set.size (Set.intersection s1 s2)) | (o1, s1) <- firstRealignments, (tf, s2) <- transformedRealignments]

    firstGoodSet :: Maybe (Coord3, Transform, Int)
    firstGoodSet = find (\(_, _, count) -> count >= 12) tfsWithCount

    makeFinalTransform :: (Coord3, Transform, Int) -> Transform
    makeFinalTransform ((x1, y1, z1), Transform offset tf, _) =
      let (x2, y2, z2) = tf offset
      in  Transform (x1 - x2, y1 - y2, z1 - z2) tf

realignSensors :: Coord3 -> [Coord3] -> Set.Set Coord3
realignSensors (fx, fy, fz) readings = Set.fromList $
  map (\(x, y, z) -> (x - fx, y - fy, z - fz)) readings

data Transform = Transform
  { offset :: Coord3
  , rotation :: TF3D
  }

makeTransformWithSet :: Coord3 -> TF3D -> Set.Set Coord3 -> (Transform, Set.Set Coord3)
makeTransformWithSet o tf realignedCoords = (Transform o tf, Set.map tf realignedCoords)

type Coord3 = (Int, Int, Int)
type TF3D = Coord3 -> Coord3

xpu (x, y, z) = (x, y, z)
xpr (x, y, z) = (x, -z, y)
xpd (x, y, z) = (x, -y, -z)
xpl (x, y, z) = (x, z, -y)
ypu (x, y, z) = (y, -x, z)
ypr (x, y, z) = (y, -z, -x)
ypd (x, y, z) = (y, x, -z)
ypl (x, y, z) = (y, z, x)
zpu (x, y, z) = (z, y, -x)
zpr (x, y, z) = (z, x, y)
zpd (x, y, z) = (z, -y, x)
zpl (x, y, z) = (z, -x, -y)
xmu (x, y, z) = (-x, -y, z)
xmr (x, y, z) = (-x, -z, -y)
xmd (x, y, z) = (-x, y, -z)
xml (x, y, z) = (-x, z, y)
ymu (x, y, z) = (-y, x, z)
ymr (x, y, z) = (-y, -z, x)
ymd (x, y, z) = (-y, -x, -z)
yml (x, y, z) = (-y, z, -x)
zmu (x, y, z) = (-z, y, x)
zmr (x, y, z) = (-z, -x, y)
zmd (x, y, z) = (-z, -y, -x)
zml (x, y, z) = (-z, x, -y)

tfs :: [TF3D]
tfs =
  [ xpu, xpr, xpd, xpl
  , xmu, xmr, xmd, xml
  , ypu, ypr, ypd, ypl
  , ymu, ymr, ymd, yml
  , zpu, zpr, zpd, zpl
  , zmu, zmr, zmd, zml
  ]

searchPairs :: [(Int, Int, Transform)] -> [(Int, [Transform])]
searchPairs inputPairs = searchTail (Seq.singleton (0, [])) (Set.singleton 0) []
  where
    searchTail :: Seq.Seq (Int, [Transform]) -> Set.Set Int -> [(Int, [Transform])] -> [(Int, [Transform])]
    searchTail queue visited accum = case Seq.viewl queue of
      Seq.EmptyL -> accum
      (n@(nextI, prevTransforms) Seq.:< restQueue) ->
        let nextPairs = filter (\(i, j, _) -> i == nextI && not (Set.member j visited)) inputPairs
            newVisited = foldl (\v (_, j, _) -> Set.insert j v) visited nextPairs
            newQueue = foldl (\q (i, j, tf) -> q Seq.|> (j, tf : prevTransforms) ) restQueue nextPairs
        in  searchTail newQueue newVisited (n : accum)

foldSensorPositions :: SensorReadingMap -> Set.Set Coord3 -> (Int, [Transform]) -> Set.Set Coord3
foldSensorPositions sensorMap prevCoords (i, tfs) = foldl (\prevSet c -> Set.insert c prevSet) prevCoords newCoords
  where
    theseCoords = sensorMap M.! i

    applyTransforms :: Coord3 -> Coord3
    applyTransforms c1 = foldl (applySingleTransform) c1 tfs

    newCoords :: [Coord3]
    newCoords = map applyTransforms theseCoords

applySingleTransform :: Coord3 -> Transform -> Coord3
applySingleTransform c1@(x1, y1, z1) (Transform (x2, y2, z2) tf) = let (x3, y3, z3) = tf c1 in (x3 + x2, y3 + y2, z3 + z2)

findFinalLocation :: (Int, [Transform]) -> (Int, Coord3)
findFinalLocation (i, tfs) = (i, foldl applySingleTransform (0, 0, 0) tfs)

-- Day 20

solvePixels :: IO ()
solvePixels = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solvePixels' fp

solvePixels' :: FilePath -> IO ()
solvePixels' fp = do
  (decodingKey, pixelSet) <- parsePixelSet fp
  let finalSet = foldl (\ps i -> expandPixelSet decodingKey ps) pixelSet [1..50]
  print $ Set.size (litCoords finalSet)

parsePixelSet :: FilePath -> IO (String, PixelSet)
parsePixelSet fp = do
  inputLines <- lines <$> readFile fp
  let initialImageLines = tail $ tail inputLines
  let numRows = length initialImageLines
  let numCols = length $ head initialImageLines
  let asArr = A.listArray ((0,0), (numRows - 1, numCols - 1))
                (concat initialImageLines)
  let lightPixels = filter (\((row,col), c) -> c == '#') (A.assocs asArr)
  let lightSet = Set.fromList (fst <$> lightPixels)
  return (head inputLines, PixelSet (0,0) (numRows - 1, numCols - 1) lightSet False)

data PixelSet = PixelSet
  { minCoords :: Coord
  , maxCoords :: Coord
  , litCoords :: Set.Set Coord
  , allElseLit :: Bool
  } deriving (Show)

isCoordLit :: Coord -> PixelSet -> Bool
isCoordLit c@(cx, cy) (PixelSet (mnx, mny) (mxx, mxy) lc allOutsideLit) =
  (Set.member c lc) || (allOutsideLit && isOutside)
  where
    isOutside = cx < mnx || cx > mxx || cy < mny || cy > mxy

expandPixelSet :: String -> PixelSet -> PixelSet
expandPixelSet decodingKey ps@(PixelSet (minX, minY) (maxX, maxY) litSet aol) =
  PixelSet (newMinX, newMinY) (newMaxX, newMaxY) newLitSet newAol
  where
    newAol = head decodingKey == '#' && not aol
    (newMinX, newMinY) = (minX - 1, minY - 1)
    (newMaxX, newMaxY) = (maxX + 1, maxY + 1)
    newTuples = [(x, y) | x <- [newMinX..newMaxX], y <- [newMinY..newMaxY]]
    newLitSet = Set.fromList $ filter (\t -> willBeLit t) newTuples

    willBeLit :: Coord -> Bool
    willBeLit c@(cx, cy) =
      let neighs = getPixelNeighbors c
          bits = map (\coord -> boolToBit (isCoordLit coord ps)) neighs
          index = bitsToDecimal64 bits
          -- Potential Optimization - Array, not list.
          decodedChar = decodingKey !! (fromIntegral index)
      in  decodedChar == '#'

getPixelNeighbors :: Coord -> [Coord]
getPixelNeighbors (r, c) =
  [ (r - 1, c - 1)
  , (r - 1, c)
  , (r - 1, c + 1)
  , (r, c - 1)
  , (r, c)
  , (r, c + 1)
  , (r + 1, c - 1)
  , (r + 1, c)
  , (r + 1, c + 1)
  ]



























