module Lib where

import Control.Monad
import Control.Monad.State
import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import System.Environment (getArgs)

-- Day 1
solveDepth :: IO ()
solveDepth = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> do
      inputLines <- readLines fp
      print (findIncreases inputLines)
      let windows = build3Windows inputLines
      print (findIncreases windows)

readLines :: FilePath -> IO [Int]
readLines fp = (map read . lines) <$> readFile fp

findIncreases :: [Int] -> Int
findIncreases [] = 0
findIncreases (first : rest) = findIncreasesTail 0 first rest
  where
    findIncreasesTail :: Int -> Int -> [Int] -> Int
    findIncreasesTail accum _ [] = accum
    findIncreasesTail accum prev (next : rest_) = if next > prev
      then findIncreasesTail (accum + 1) next rest_
      else findIncreasesTail accum next rest_

build3Windows :: [Int] -> [Int]
build3Windows inputs = if length inputs < 3 then []
  else zipWith (+) (zipWith (+) inputs (drop 1 inputs)) (drop 2 inputs)

-- Day 2

data Move =
  Up Int |
  Down Int |
  Forward Int

solvePosition :: IO ()
solvePosition = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> do
      moves <- readMoves fp
      let (horizontal, vertical) = calculateFinalPositions' moves
      print (horizontal, vertical)
      print (horizontal * vertical)

readMoves :: FilePath -> IO [Move]
readMoves fp = (map convertLine . lines) <$> readFile fp

convertLine :: String -> Move
convertLine input = case startWord of
  "forward" -> Forward value
  "up" -> Up value
  "down" -> Down value
  _ -> error $ "Invalid input: " ++ input
  where
    startWord = takeWhile (/= ' ') input
    value = read $ tail (dropWhile (/= ' ') input)

-- Input: List of moves
-- Output: Final Horizontal Position, Final Vertical Position
calculateFinalPositions :: [Move] -> (Int, Int)
calculateFinalPositions = calculateTail (0, 0)
  where
    calculateTail :: (Int, Int) -> [Move] -> (Int, Int)
    calculateTail accum [] = accum
    calculateTail (accumHorizontal, accumVertical) (move : rest) = case move of
      Up val -> calculateTail (accumHorizontal, accumVertical - val) rest
      Down val -> calculateTail (accumHorizontal, accumVertical + val) rest
      Forward val -> calculateTail (accumHorizontal + val, accumVertical) rest

-- Input: List of moves
-- Output: Final Horizontal Position, Final Vertical Position
calculateFinalPositions' :: [Move] -> (Int, Int)
calculateFinalPositions' = calculateTail (0, 0, 0)
  where
    --               Horiz Vert Aim
    calculateTail :: (Int, Int, Int) -> [Move] -> (Int, Int)
    calculateTail (horiz, vert, _) [] = (horiz, vert)
    calculateTail (accumHoriz, accumVert, aim) (move : rest) = case move of
      Up val -> calculateTail (accumHoriz, accumVert, aim - val) rest
      Down val -> calculateTail (accumHoriz, accumVert, aim + val) rest
      Forward val ->
        calculateTail (accumHoriz + val, accumVert + (aim * val), aim) rest




-- Day 3
solvePower :: FilePath -> IO ()
solvePower fp = do
  binaryStrings <- lines <$> readFile fp
  let resultMap = processStrings binaryStrings
  let gamma = produceGamma resultMap (length binaryStrings)
  let epsilon = produceEpsilonFromGamma gamma
  let gammaNum = binaryStringToNumber gamma
  let epsilonNum = binaryStringToNumber epsilon
  print (gammaNum, epsilonNum)
  print $ gammaNum * epsilonNum
  let oxyGen = findRatingString True binaryStrings
  let co2Scrubber = findRatingString False binaryStrings
  print (oxyGen, co2Scrubber)
  let oxyGenNum = binaryStringToNumber $ head oxyGen
  let co2ScrubberNum = binaryStringToNumber $ head co2Scrubber
  print (oxyGenNum, co2ScrubberNum)
  print $ oxyGenNum * co2ScrubberNum

processStrings :: [String] -> M.Map Int Int
processStrings = foldl processString M.empty
  where
    processString :: M.Map Int Int -> String -> M.Map Int Int
    processString prevMap inputLine = foldl processBit prevMap (zip [0,1..] inputLine)

    processBit :: M.Map Int Int -> (Int, Char) -> M.Map Int Int
    processBit prevMap (index, char) = case char of
      '1' -> incrementKey index prevMap
      _ -> prevMap


incrementKey :: (Ord k) => k -> M.Map k Int -> M.Map k Int
incrementKey i prevMap = case M.lookup i prevMap of
  Nothing -> M.insert i 1 prevMap
  Just j -> M.insert i (j + 1) prevMap


produceGamma :: M.Map Int Int -> Int -> String
produceGamma bitMap numStrings = map gammaChar [0..maxIndex]
  where
    maxIndex = fst $ M.findMax bitMap

    gammaChar :: Int -> Char
    gammaChar i = case M.lookup i bitMap of
      Nothing -> '0'
      Just numberOf1s -> if numberOf1s > (numStrings `quot` 2)
        then '1'
        else '0'

produceEpsilonFromGamma :: String -> String
produceEpsilonFromGamma gamma = map flipChar gamma
  where
    flipChar '1' = '0'
    flipChar _ = '1'

binaryStringToNumber :: String -> Integer
binaryStringToNumber input = tailHelper (reverse input) 0 1
  where
    tailHelper :: String -> Integer -> Integer -> Integer
    tailHelper [] accum _ = accum
    tailHelper (nextChar : rest) accum power = tailHelper
      rest
      (if nextChar == '1' then accum + power else accum)
      (power * 2)

findRatingString :: Bool -> [String] -> [String]
findRatingString shouldMatchMostCommon inputs = foldl
  foldingFunc
  inputs
  [0..(length (head inputs))]
  where
    foldingFunc :: [String] -> Int -> [String]
    foldingFunc inputs index =
      filterByBitPosition index shouldMatchMostCommon inputs

filterByBitPosition :: Int -> Bool -> [String] -> [String]
filterByBitPosition _ _ [] = []
filterByBitPosition _ _ [final] = [final]
filterByBitPosition index shouldMatchMostCommon inputs = filter filterFunc inputs
  where
    numStrings = length inputs
    bitMap = processStrings inputs
    numberOf1s = case M.lookup index bitMap of
      Nothing -> 0
      Just i -> i
    numberOf0s = numStrings - numberOf1s
    mostCommon = if numberOf1s >= numberOf0s then '1' else '0'
    filterFunc :: String -> Bool
    -- Should XOR this
    filterFunc thisString =
      (thisString !! index == mostCommon && shouldMatchMostCommon) ||
      (thisString !! index /= mostCommon && not shouldMatchMostCommon)

-- Day 5


-- Parse Numbers to Call
-- Parse Boards
-- "Call" each number 
--    - Go through each board, mark spaces as "Marked"
--    - Calculate if a board has won
--    - Get the score on the board.

newtype Board = Board (A.Array (Word, Word) (Int, Bool))
  deriving (Show)

data GameState = GameState
  { numbersToCall :: [Int]
  , gameBoards :: [Board]
  }

solveBingo :: IO ()
solveBingo = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveBingo' fp

solveBingo' :: FilePath -> IO ()
solveBingo' fp = do
  (numbersToCall, boards) <- parseInputs fp
  -- print numbersToCall
  -- print (head boards)
  let gameState = GameState numbersToCall boards
  let winner = playGame gameState
  case winner of
    Nothing -> print "No one won :("
    Just winner -> do
      --print $ fst winner
      -- print $ snd winner
      print (scoreForWinningBoard winner)

parseInputs :: FilePath -> IO ([Int], [Board])
parseInputs fp = do
  inputLines <- lines <$> readFile fp
  case inputLines of
    [] -> print "Invalid Input" >> return ([], [])
    (firstLine : rest) -> do
      let numbersToCall = parseCSVInts firstLine
      let boards = parseBoards rest
      return (numbersToCall, boards)

-- Return Winning Board
playGame :: GameState -> Maybe (Board, Int)
playGame = evalState playRound 

playRound :: State GameState (Maybe (Board, Int))
playRound = do
  numbers <- gets numbersToCall
  case numbers of
    [] -> return Nothing
    (nextNumber : restNumbers) -> do
      markBoards nextNumber
      gs <- get
      let boards = gameBoards gs
      if length boards == 1
        then if boardHasWon (head boards) then return $ Just (head boards, nextNumber)
          else put (gs { numbersToCall = restNumbers }) >> playRound
        else do
          let nonWinningBoards = filter (not . boardHasWon) boards
          put (GameState restNumbers nonWinningBoards) >> playRound
      {-let winningBoards = filter boardHasWon (gameBoards gs)
      case winningBoards of
        [] -> put (gs { numbersToCall = restNumbers }) >> playRound
        (winningBoard : _) -> return $ Just (winningBoard, nextNumber)-}
  
markBoards :: Int -> State GameState ()
markBoards number = do
  gs <- get
  let newBoards = map (markSingleBoard number) (gameBoards gs)
  put (gs { gameBoards = newBoards } )

type BoardAssoc = ((Word, Word), (Int, Bool))

markSingleBoard :: Int -> Board -> Board
markSingleBoard number (Board boardArray) = Board $
  boardArray A.// (map updateAssoc matchingAssocs)
  where
    matchesNumber :: BoardAssoc -> Bool
    matchesNumber (_, (val, _)) = val == number

    matchingAssocs :: [BoardAssoc]
    matchingAssocs = filter matchesNumber (A.assocs boardArray)

    updateAssoc :: BoardAssoc -> BoardAssoc
    updateAssoc ((r, c), (val, _)) = ((r, c), (val, True))

boardHasWon :: Board -> Bool
boardHasWon (Board boardArray) =
  or ((all isMarked) <$> rows) || or ((all isMarked <$> columns))
  where
    isMarked :: BoardAssoc -> Bool
    isMarked (_, (_, b)) = b

    rowEquals :: Word -> BoardAssoc -> Bool
    rowEquals i ((x, _), _) = i == x

    columnEquals :: Word -> BoardAssoc -> Bool
    columnEquals i ((_, x), _) = i == x

    rows :: [[BoardAssoc]]
    rows = map (\i -> filter (rowEquals i) (A.assocs boardArray)) [0..4]

    columns :: [[BoardAssoc]]
    columns = map (\i -> filter (columnEquals i) (A.assocs boardArray)) [0..4]

scoreForWinningBoard :: (Board, Int) -> Int
scoreForWinningBoard (Board boardArray, finalNumber) =
  finalNumber * sum (fst <$> unmarked)
  where
    isUnmarked :: (Int, Bool) -> Bool
    isUnmarked (_, b) = not b
 
    unmarked :: [(Int, Bool)]
    unmarked = filter isUnmarked (A.elems boardArray)

-- Done
parseCSVInts :: String -> [Int]
parseCSVInts input = read <$> splitOn "," input

parseBoards :: [String] -> [Board]
parseBoards = parseBoards' []
  where
    lineToNumbers :: String -> [Int]
    lineToNumbers input = read <$> words input

    parseBoards' :: [Board] -> [String] -> [Board]
    parseBoards' boards inputLines = if length inputLines < 6 then boards
      else let newBoard = createBoard (lineToNumbers <$> (take 5 (tail inputLines)))
            in parseBoards' (newBoard : boards) (drop 6 inputLines)

    createBoard :: [[Int]] -> Board
    createBoard inputs = Board $ A.listArray
      ((0,0), (4,4))
      (zip (concat inputs) (repeat False))


-- Day 5

type Coord = (Int, Int)
data Vent = Vent
  { ventStart :: Coord
  , ventEnd :: Coord
  } deriving (Show)

isVerticalVent :: Vent -> Bool
isVerticalVent (Vent (x1, y1) (x2, y2)) = x1 == x2

isHorizontalVent :: Vent -> Bool
isHorizontalVent (Vent (x1, y1) (x2, y2)) = y1 == y2

solveVents :: IO ()
solveVents = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveVents' fp

solveVents' :: FilePath -> IO ()
solveVents' fp = do
  vents <- parseVents fp
  -- let horizOrVertVents = filter (\v -> isHorizontalVent v || isVerticalVent v) vents
  let ventMap = buildMap vents
  print $ length $ filter (>= 2) (M.elems ventMap)

parseVents :: FilePath -> IO [Vent]
parseVents fp = (map parseSingleVent . lines) <$> readFile fp

parseSingleVent :: String -> Vent
parseSingleVent input = case splitOn " " input of
  [firstTuple, _, secondTuple] -> Vent
    (read (addParens firstTuple)) (read (addParens secondTuple))
  _ -> error "Bad input"
  where
    addParens str = '(' : (str ++ ")")

buildMap :: [Vent] -> M.Map Coord Int
buildMap = foldl f M.empty
  where
    f :: M.Map Coord Int -> Vent -> M.Map Coord Int
    f prevMap v = foldl
      (\prevMap' coord -> incrementKey coord prevMap')
      prevMap
      (coordsInVent v)

coordsInVent :: Vent -> [Coord]
coordsInVent v@(Vent (x1, y1) (x2, y2)) = if isVerticalVent v
  then map (\i -> (x1, i)) [(min y1 y2)..(max y1 y2)]
  else if isHorizontalVent v
    then map (\i -> (i, y1)) [(min x1 x2)..(max x1 x2)]
    else
      let xsBase = [(min x1 x2)..(max x1 x2)]
          ysBase = [(min y1 y2)..(max y1 y2)]
          xs = if x1 < x2 then xsBase else reverse xsBase
          ys = if y1 < y2 then ysBase else reverse ysBase
      in  zip xs ys

maxMapValue :: (Ord v, Ord k, Bounded v) => M.Map k v -> v
maxMapValue valueMap = case M.elems valueMap of
  [] -> minBound
  es -> maximum es

countElems :: (Ord k, Eq v) => v -> M.Map k v -> Int
countElems elem valueMap = length $ filter (== elem) (M.elems valueMap)


-- Day 6

data FishState = FishState
  { t0 :: Integer
  , t1 :: Integer
  , t2 :: Integer
  , t3 :: Integer
  , t4 :: Integer
  , t5 :: Integer
  , t6 :: Integer
  , t7 :: Integer
  , t8 :: Integer
  } deriving (Show)

solveLanternFish :: IO ()
solveLanternFish = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveLanternFish' fp

solveLanternFish' :: FilePath -> IO ()
solveLanternFish' fp = do
  initialNumbers <- (parseCSVInts . head . lines) <$> readFile fp
  let initialFishState = produceInitialFishState initialNumbers
  let finalFish = evalState (runDay 256) initialFishState
  print finalFish

produceInitialFishState :: [Int] -> FishState
produceInitialFishState = foldl pifs (FishState 0 0 0 0 0 0 0 0 0)
  where
    pifs :: FishState -> Int -> FishState
    pifs fs i = case i of
      0 -> fs { t0 = t0 fs + 1 }
      1 -> fs { t1 = t1 fs + 1 }
      2 -> fs { t2 = t2 fs + 1 }
      3 -> fs { t3 = t3 fs + 1 }
      4 -> fs { t4 = t4 fs + 1 }
      5 -> fs { t5 = t5 fs + 1 }
      6 -> fs { t6 = t6 fs + 1 }
      7 -> fs { t7 = t7 fs + 1 }
      8 -> fs { t8 = t8 fs + 1 }
      _ -> fs

runDay :: Int -> State FishState Integer
runDay 0 = do
  (FishState f0 f1 f2 f3 f4 f5 f6 f7 f8) <- get
  return $ f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8
runDay daysRemaining = do
  oldState <- get
  let newState = updateFishState oldState
  put newState
  runDay (daysRemaining - 1)

updateFishState :: FishState -> FishState
updateFishState (FishState f0 f1 f2 f3 f4 f5 f6 f7 f8) = FishState
  f1 f2 f3 f4 f5 f6 (f7 + f0) f8 f0

produceNewList :: [Int] -> [Int]
produceNewList = pnl []
  where
    pnl :: [Int] -> [Int] -> [Int]
    pnl accum [] = reverse accum
    pnl accum (nextFish : restFish) = if nextFish == 0
      then pnl (8 : 6 : accum) restFish
      else pnl ((nextFish - 1) : accum) restFish


-- Day 7


solveCrabs :: IO ()
solveCrabs = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveCrabs' fp

solveCrabs' :: FilePath -> IO ()
solveCrabs' fp = do
  numbers <- (parseCSVInts . head . lines) <$> readFile fp
  let maxIndex = maximum numbers
  let minIndex = minimum numbers
  let crabMap = createIncidenceMap numbers
  -- let totalCrabs = length numbers
  -- let answer = solveCrabState crabMap totalCrabs minIndex maxIndex
  let answer = solveCrab2 crabMap minIndex maxIndex
  print answer

-- The minimum fuel cost so far
-- The previous cost
-- Current index i
-- The sum of the number of crabs to the left of i
data CrabState = CrabState
  { minimumFuelCost :: Int
  , previousCost    :: Int
  , numberOfCrabsToLeft :: Int
  }

createIncidenceMap :: (Ord v) => [v] -> M.Map v Int
createIncidenceMap = foldl cim M.empty
  where
    cim :: (Ord v) => M.Map v Int -> v -> M.Map v Int
    cim prevMap value = case M.lookup value prevMap of
      Nothing -> M.insert value 1 prevMap
      Just i -> M.insert value (i + 1) prevMap

countIncidences :: (Ord v) => v -> M.Map v Int -> Int
countIncidences value incidenceMap = case M.lookup value incidenceMap of
  Nothing -> 0
  Just j -> j

-- Answer is minimum fuel cost
solveCrabState :: M.Map Int Int -> Int -> Int -> Int -> Int
solveCrabState crabMap totalCrabs minIndex maxIndex = minimumFuelCost $
  foldl (crabFold crabMap totalCrabs) initialState [(minIndex + 1)..maxIndex]
  where
    crabsAtMin = countIncidences minIndex crabMap
    initialState = CrabState initialCost initialCost crabsAtMin

    initialCost = sum $ map
      (\(index, count) -> count * (index - minIndex)) (M.toList crabMap)

crabFold :: M.Map Int Int -> Int -> CrabState -> Int -> CrabState
crabFold crabMap totalCrabs (CrabState mfc prev toLeft) i =
  CrabState (min mfc fuelCostHere) fuelCostHere (toLeft + atThisIndex)
  where
    fuelCostHere = prev + toLeft - (totalCrabs - toLeft)
    atThisIndex = countIncidences i crabMap

costAtIndex :: M.Map Int Int -> Int -> Int
costAtIndex crabMap i = sum $ map
      (\(index, count) -> count * (triangleNumber $ abs (index - i)))
      (M.toList crabMap)

triangleNumber :: Int -> Int
triangleNumber n = n * (n + 1) `quot` 2

solveCrab2 :: M.Map Int Int -> Int -> Int -> Int
solveCrab2 crabMap minIndex maxIndex = minimum
  (map (costAtIndex crabMap) [minIndex..maxIndex])


-- Day 8

solveDisplays :: IO ()
solveDisplays = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveDisplays' fp

solveDisplays' :: FilePath -> IO ()
solveDisplays' fp = do
  patterns <- (map parseSensorLine . lines) <$> readFile fp
  -- forM_ inputLines $ \(wp, dp) -> print wp >> print dp >> putStrLn ""
  -- let displayPatterns = snd <$> patterns
  -- let numDistinct = sum (countDistinct <$> displayPatterns)
  -- print $ solvePattern
    -- (WirePattern "acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab", DisplayPattern "cdfeb" "fcadb" "cdfeb" "cdbaf")
  print $ sum $ map solvePattern patterns

data WirePattern = WirePattern
  { wp1 :: String
  , wp2 :: String
  , wp3 :: String
  , wp4 :: String
  , wp5 :: String
  , wp6 :: String
  , wp7 :: String
  , wp8 :: String
  , wp9 :: String
  , wp10 :: String
  } deriving (Show)

data DisplayPattern = DisplayPattern
  { dp1 :: String
  , dp2 :: String
  , dp3 :: String
  , dp4 :: String
  } deriving (Show)

data WireMapping = WireMapping
  { wmA :: Char
  , wmB :: Char
  , wmC :: Char
  , wmD :: Char
  , wmE :: Char
  , wmF :: Char
  , wmG :: Char
  } deriving (Show)

parseSensorLine :: String -> (WirePattern, DisplayPattern)
parseSensorLine input = case splitOn " | " input of
  [wirePatternString, displayPatternString] ->
    let wpList = words wirePatternString
        dpList = words displayPatternString
    in  case (wpList, dpList) of
      ([w1, w2, w3, w4, w5, w6, w7, w8, w9, w10], [d1, d2, d3, d4]) ->
        (WirePattern w1 w2 w3 w4 w5 w6 w7 w8 w9 w10, DisplayPattern d1 d2 d3 d4)
      _ -> error "Invalid Input"
  _ -> error $ "Invalid input: " ++ input

countDistinct :: DisplayPattern -> Int
countDistinct (DisplayPattern d1 d2 d3 d4) = length $
  filter isDistinct [d1, d2, d3, d4]
  where
    isDistinct :: String -> Bool
    isDistinct s =
      length s == 2 || length s == 3 || length s == 4 || length s == 7

solvePattern :: (WirePattern, DisplayPattern) -> Int
solvePattern (wp, dp) = findNumberOnDisplay mapping dp
  where
    mapping = createMappingFromPattern wp

createMappingFromPattern :: WirePattern -> WireMapping
createMappingFromPattern (WirePattern w1 w2 w3 w4 w5 w6 w7 w8 w9 w10) =
  WireMapping aChar bChar cChar dChar eChar fChar gChar
  where
    allStrings = [w1,w2,w3,w4,w5,w6,w7,w8,w9,w10]
    sortedByLength = sortOn length allStrings
    [one, seven, four, l51, l52, l53, l61, l62, l63, eight] = sortedByLength
    [aChar] = sort seven \\ sort one
    [cOrF1, cOrF2] = one
    (cChar, fChar) = if all (elem cOrF1) [l61, l62, l63] then (cOrF2, cOrF1)
      else (cOrF1, cOrF2)
    [bOrG1, bOrG2] = delete aChar $ delete fChar $ intersect l63 (intersect l61 l62)
    (bChar, gChar) = if bOrG1 `elem` (delete fChar $ delete cChar four)
      then (bOrG1, bOrG2) else (bOrG2, bOrG1)
    dChar = head (sort four \\ sort [bChar, cChar, fChar])
    eChar = head $ eight \\ sort [aChar, bChar, cChar, dChar, fChar, gChar]

findNumberOnDisplay :: WireMapping -> DisplayPattern -> Int
findNumberOnDisplay mapping (DisplayPattern d1 d2 d3 d4) =
  i1 * 1000 + i2 * 100 + i3 * 10 + i4
  where
    i1 = convertRawString $ createRawString mapping d1
    i2 = convertRawString $ createRawString mapping d2
    i3 = convertRawString $ createRawString mapping d3
    i4 = convertRawString $ createRawString mapping d4

createRawString :: WireMapping -> String -> String
createRawString mapping input = map convertChar input
  where
    convertChar c
      | c == wmA mapping = 'a'
      | c == wmB mapping = 'b'
      | c == wmC mapping = 'c'
      | c == wmD mapping = 'd'
      | c == wmE mapping = 'e'
      | c == wmF mapping = 'f'
      | c == wmG mapping = 'g'
      | otherwise = error "Invalid char!"

convertRawString :: String -> Int
convertRawString input = case sort input of
  "abcefg" -> 0
  "cf" -> 1
  "acdeg" -> 2
  "acdfg" -> 3
  "bcdf" -> 4
  "abdfg" -> 5
  "abdefg" -> 6
  "acf" -> 7
  "abcdefg" -> 8
  "abcdfg" -> 9
  _ -> error $ "Invalid raw input! " ++ sort input

-- Day 9

solveLava :: IO ()
solveLava = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveLava' fp

solveLava' :: FilePath -> IO ()
solveLava' fp = do
  digitsArray <- read2DDigits fp
  let lowNumbers = findLowNumbers digitsArray
  -- print $ sum (map (+ 1) (snd <$> lowNumbers))
  let basinSizes = map (findBasinSize digitsArray) (fst <$> lowNumbers)
  let largest3 = take 3 (reverse $ sort basinSizes)
  print $ product largest3

-- Reuse me!
read2DDigits :: FilePath -> IO (A.Array (Int, Int) Int)
read2DDigits fp = do
  inputLines <- lines <$> readFile fp
  when (null inputLines) (putStrLn "Empty Input!")
  let numRows = length inputLines
  let numCols = length $ head inputLines
  let numbers = concatMap lineToNumbers inputLines
  return $ A.listArray ((0, 0), (numRows - 1, numCols - 1)) numbers
  where
    lineToNumbers :: String -> [Int]
    lineToNumbers inputLine = map (\c -> read [c]) inputLine

findLowNumbers :: A.Array (Int, Int) Int -> [((Int, Int), Int)]
findLowNumbers arr = filter isLowNumber (A.assocs arr)
  where
    (maxRow, maxCol) = snd $ A.bounds arr

    isLowNumber :: ((Int, Int), Int) -> Bool
    isLowNumber (coords, i) = all (\n -> arr A.! n > i)
      (getNeighbors (maxRow, maxCol) coords)

getNeighbors :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getNeighbors (maxRow, maxCol) (r, c) =
  let maybeUp = if r > 0 then Just (r - 1, c) else Nothing
      maybeLeft = if c > 0 then Just (r, c - 1) else Nothing
      maybeDown = if r < maxRow then Just (r + 1, c) else Nothing
      maybeRight = if c < maxCol then Just (r, c + 1) else Nothing
  in  catMaybes [maybeUp, maybeLeft, maybeDown, maybeRight]

findBasinSize :: A.Array (Int, Int) Int -> (Int, Int) -> Int
findBasinSize arr loc = evalState (runBfs arr)
  (BFSState (Set.singleton loc) (Seq.singleton loc))

runBfs :: A.Array (Int, Int) Int -> State BFSState Int
runBfs arr = do
  (BFSState v q) <- get
  case Seq.viewl q of
    Seq.EmptyL -> return (Set.size v)
    topLoc Seq.:< rest -> do
      let maxes = snd $ A.bounds arr
      let neighbors = getNeighbors maxes topLoc
      let lowNeighbors = filter
                           (\c -> arr A.! c /= 9 && not (Set.member c v))
                           neighbors
      let newQueue = foldl (\queue n -> queue Seq.|> n) rest lowNeighbors
      let newVisited = foldl (\st n -> Set.insert n st) v lowNeighbors
      put $ BFSState newVisited newQueue
      runBfs arr

data BFSState = BFSState
  { visited :: Set.Set Coord
  , queue :: Seq.Seq Coord
  }

-- Day 10

solveNavigation :: IO ()
solveNavigation = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveNavigation' fp

solveNavigation' :: FilePath -> IO ()
solveNavigation' fp = do
  inputLines <- lines <$> readFile fp
  let results = map findCorruptCharAndStack inputLines
  let invalidScore = sum $ map scoreInvalidChar $
                       catMaybes (fst <$> results)
  print invalidScore
  let incomplete = filter (\(corrupt, _) -> corrupt == Nothing) results
  let scores = map (scoreLine . completeLine) (snd <$> incomplete)
  print $ median scores

median :: (Ord a) => [a] -> a
median [] = error "Cannot take median of empty"
median inputs = sort inputs !! (length inputs `quot` 2)

findCorruptCharAndStack :: String -> (Maybe Char, Seq.Seq Char)
findCorruptCharAndStack input = runState (runNavParse input) Seq.empty

runNavParse :: String -> State (Seq.Seq Char) (Maybe Char)
runNavParse [] = return Nothing
runNavParse (firstChar : rest) =
  -- Is this an "opening char" ( { [ <
  if firstChar `elem` openingChars
    -- If so put on top our our stack and recurse
    then modify ((Seq.<|) firstChar) >> runNavParse rest
    -- If not...does it match the top of the stack?
    else do
      currentStack <- get
      case Seq.viewl currentStack of
        Seq.EmptyL -> return $ Just firstChar
        topChar Seq.:< restStack -> if navCharMatches topChar firstChar
          --    If yes, pop the stack, recurse
          then put restStack >> runNavParse rest
          --    If no, return firstChar
          else return $ Just firstChar

openingChars :: [Char]
openingChars = "([{<"

navCharMatches :: Char -> Char -> Bool
navCharMatches '(' ')' = True
navCharMatches '[' ']' = True
navCharMatches '{' '}' = True
navCharMatches '<' '>' = True
navCharMatches _ _ = False


matchingChar :: Char -> Char
matchingChar '(' = ')'
matchingChar '[' = ']'
matchingChar '{' = '}'
matchingChar '<' = '>'
matchingChar c = error $ "Non-opening char: " ++ show c

scoreInvalidChar :: Char -> Int
scoreInvalidChar ')' = 3
scoreInvalidChar ']' = 57
scoreInvalidChar '}' = 1197
scoreInvalidChar '>' = 25137
scoreInvalidChar _ = 0

completeLine :: Seq.Seq Char -> String
completeLine charSeq = map matchingChar (F.toList charSeq)

scoreLine :: String -> Int
scoreLine = scoreLineTail 0
  where
    scoreLineTail :: Int -> String -> Int
    scoreLineTail prevScore [] = prevScore
    scoreLineTail prevScore (c : rest) = case c of
      ')' -> scoreLineTail (5 * prevScore + 1) rest
      ']' -> scoreLineTail (5 * prevScore + 2) rest
      '}' -> scoreLineTail (5 * prevScore + 3) rest
      '>' -> scoreLineTail (5 * prevScore + 4) rest

-- Day 11


solveEnergy :: IO ()
solveEnergy = do
  args <- getArgs
  case args of
    [] -> print "No input file!"
    (fp : _) -> solveEnergy' fp

solveEnergy' :: FilePath -> IO ()
solveEnergy' fp = do
  octopusArray <- read2DDigits fp
  print $ findTotalEnergy octopusArray
  print $ findFirstFlash octopusArray

type OctoArray = A.Array (Int, Int) Int

-- Run 100 steps of the operation
findTotalEnergy :: OctoArray -> Int
findTotalEnergy oArray = fst $ foldl f (0, oArray) [1..100]
  where
    f :: (Int, OctoArray) -> Int -> (Int, OctoArray)
    f (accum, prevOArray) _ =
      let (flashesThisStep, newArray, _) = runStep prevOArray
      in  (accum + flashesThisStep, newArray)

findFirstFlash :: OctoArray -> Int
findFirstFlash oArray = fff oArray [1..1000]
  where
    fff :: OctoArray -> [Int] -> Int
    fff _ [] = minBound
    fff prevOArray (step : steps) =
      let (_, newArray, allFlashed) = runStep prevOArray
      in  if allFlashed then step else fff newArray steps

runStep :: OctoArray -> (Int, OctoArray, Bool)
runStep oArray = (length allFlashes, finalArray, allFlashed)
  -- Start by incrementing everything
  where
    incrementedArray = A.listArray (A.bounds oArray)
      (map (+ 1) (A.elems oArray))
    initialFlashes = fst <$>
      filter (\(coord, x) -> x >= 10) (A.assocs incrementedArray)
    initialOes = OctoEnergyState
      (Set.fromList initialFlashes) (Seq.fromList initialFlashes)
    processedFlashes = evalState (processFlashes incrementedArray) initialOes
    allFlashes = filter (\(coord, x) -> x >= 10) (A.assocs processedFlashes)
    finalModifications = map (\(coord, _) -> (coord, 0)) allFlashes
    finalArray = processedFlashes A.// finalModifications
    allFlashed = length allFlashes == length (A.elems oArray)

data OctoEnergyState = OctoEnergyState
  { oesVisited :: Set.Set Coord
  , oesQueue :: Seq.Seq Coord
  } deriving (Show)

processFlashes :: OctoArray -> State OctoEnergyState OctoArray
processFlashes prevArray = do
  (OctoEnergyState v q) <- get
  -- If queue is empty, we have processed everything, return this array
  case Seq.viewl q of
    Seq.EmptyL -> return prevArray
    (firstLoc Seq.:< restQ) -> do
      let allNeighbors = getAllNeighbors (snd $ A.bounds prevArray) firstLoc
      let cellsToAdd = filter
                        (shouldAddToQueue v)
                        allNeighbors
      -- Create Modified Array
      let modifications = map (\c -> (c, (prevArray A.! c) + 1)) allNeighbors
      let newArray = prevArray A.// modifications
      -- Modify the Visited Set
      let newVisited = foldl (\st n -> Set.insert n st) v cellsToAdd
      -- Modify the Queue
      let newQueue = foldl (\queue n -> queue Seq.|> n) restQ cellsToAdd
      put $ OctoEnergyState newVisited newQueue
      processFlashes newArray
  where
    shouldAddToQueue :: Set.Set Coord -> Coord -> Bool
    shouldAddToQueue visited c = not (Set.member c visited) &&
        prevArray A.! c >= 9

-- Includes diagonals, unlike getNeighbors
getAllNeighbors :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getAllNeighbors (maxRow, maxCol) (r, c) =
  let maybeUp = if r > 0 then Just (r - 1, c) else Nothing
      maybeUL = if r > 0 && c > 0 then Just (r - 1, c - 1) else Nothing
      maybeLeft = if c > 0 then Just (r, c - 1) else Nothing
      maybeDL = if c > 0 && r < maxRow then Just (r + 1, c - 1) else Nothing
      maybeDown = if r < maxRow then Just (r + 1, c) else Nothing
      maybeDR = if r < maxRow && c < maxCol then Just (r + 1, c + 1) else Nothing
      maybeRight = if c < maxCol then Just (r, c + 1) else Nothing
      maybeUR = if c < maxCol && r > 0 then Just (r - 1, c + 1) else Nothing
  in  catMaybes [maybeUp, maybeUL, maybeLeft, maybeDL, maybeDown, maybeDR, maybeRight, maybeUR]














