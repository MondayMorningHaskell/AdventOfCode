{-# LANGUAGE OverloadedStrings #-}

module Day22 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1, some, (<|>), MonadParsec (eof))
import Text.Megaparsec.Char (eol, char)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, Grid2, Coord2, parsePositiveNumber)
import qualified Data.Array as A
import Data.List (find, sortOn, findIndex, sort)
import Control.Monad (forM_)
import Control.Monad.Cont (lift)

dayNum :: Int
dayNum = 22

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO (Maybe Int)
solveEasy fp = runStdoutLoggingT $ do
  input@((grid, rowInfos, columnInfos), turns) <- parseFile parseInput fp
  result <- processInputEasy input
  findEasySolution result

solveHard :: String -> FilePath -> IO (Maybe Int)
solveHard size fp = runStdoutLoggingT $ do
  input@((grid, _, _), turns) <- parseFile parseInput fp
  -- This problem requires hardcoding between small and large solutions.
  let (wrapFunc, faceFunc) = if size == "small" then (wrapEasy, getFaceEasy) else (wrapHard, getFaceHard)
  result <- processInputHard ((grid, faceFunc), turns) wrapFunc
  findEasySolution result -- < Evaluation solution is same as in the "Easy" part.

-------------------- PARSING --------------------
data Direction =
  FaceUp |
  FaceDown |
  FaceLeft |
  FaceRight
  deriving (Show, Eq)

data Turn = TurnLeft | TurnRight
  deriving (Show, Eq)

data Cell =
  Empty |
  Wall |
  Blank
  deriving (Show, Eq)

type MazeInfo = (Grid2 Cell, A.Array Int (Int, Int), A.Array Int (Int, Int))
type InputType = (MazeInfo, [(Turn, Int)])

-- 1. Parse the cells lines (which adds padding to the front).
-- 2. Get the maximum column and add padding to the back.
--      This includes one Blank beyond the final column for every row.
-- 3. Add an extra line of padding of 'Blank' to the top and bottom.
-- 4. Construct a 2D Array with the cells.
--      First element that can be in the maze is (1,1), but Array's index starts at (0,0) for padding.
-- 5. Make an array out of "rowInfos", which are included from parsing the rows.
--      These tell us the first and last non-Blank index in each row.
-- 6. Calculate "columnInfos" based on the maze grid.
--      These tell us the first and last non-Blank index in each column.
-- 7. Parse the path.
parseInput :: (MonadLogger m, MonadFail m) => ParsecT Void Text m InputType
parseInput = do
  {- 1 -}
  cellLines <- sepEndBy1 parseLine eol
  let maxColumn = maximum (snd . snd <$> cellLines)
{-2-} paddedCellLines = map (\(cells, (_, lastNonBlankIndex)) -> cells ++ replicate (maxColumn - lastNonBlankIndex + 1) Blank) cellLines
{-3-} topBottom = replicate (maxColumn + 2) Blank
      finalCells = concat (topBottom : paddedCellLines) ++ topBottom
{-4-} maze = A.listArray ((0, 0), (length paddedCellLines + 1, maxColumn + 1)) finalCells
{-5-} rowInfos = A.listArray (1, length cellLines) (snd <$> cellLines)
{-6-} columns = map (calculateColInfo maze) [1..maxColumn]
      columnInfos = A.listArray (1, maxColumn) columns
  eol
  {-7-}
  firstLength <- parsePositiveNumber
  path <- parsePath [(TurnRight, firstLength)]
  return ((maze, rowInfos, columnInfos), path)
  where
    {- 6 -}
    calculateColInfo :: Grid2 Cell -> Int -> (Int, Int)
    calculateColInfo maze col =
      let nonBlankAssocs = filter (\((r, c), cell) -> c == col && cell /= Blank) (A.assocs maze)
          sorted = sort $ fst . fst <$> nonBlankAssocs
      in  (head sorted, last sorted)

-- Parse the directions.
-- This function is recursive. It runs until we encounter 'eof'.
parsePath :: (MonadLogger m, MonadFail m) => [(Turn, Int)] -> ParsecT Void Text m [(Turn, Int)]
parsePath accum = finished <|> notFinished
  where
    finished = eof >> return (reverse accum) {- Base Case: End-of--File -}
    notFinished = do
      t <- (char 'R' >> return TurnRight) <|> (char 'L' >> return TurnLeft)
      i <- parsePositiveNumber
      parsePath ((t, i) : accum) {- Recursive Case: Add the new turn and distance. -}

-- Parse a single line of maze input.
-- '.' = Empty space in the maze
-- '#' = A wall in the maze
-- ' ' = A 'Blank' space that is not part of the maze.
--
-- In addition to the list of cells, this also returns the start and end
-- column of the non-blank spaces.
--
-- NOTE: This function adds an extra 'Blank' to the front of the
--       row because we want to pad all 4 directions.
type LineType = ([Cell], (Int, Int))
parseLine :: (MonadLogger m, MonadFail m) => ParsecT Void Text m LineType
parseLine = do
  cells <- some parseCell
  let frontPadded = Blank : cells
  case findIndex (/= Blank) frontPadded of
    Nothing -> fail "A line is completely blank!"
    Just i -> do
      return (frontPadded, (i, length frontPadded - 1))
  where
    parseCell = (char ' ' >> return Blank) <|> (char '.' >> return Empty) <|> (char '#' >> return Wall)

-------------------- SOLVING EASY --------------------
type EasySolutionType = (Coord2, Direction)

processInputEasy :: (MonadLogger m) => (MazeInfo, [(Turn, Int)]) -> m EasySolutionType
processInputEasy (info@(maze, _, _), directions) = runMoves info (start, FaceUp) directions
  where
    -- The initial position in the maze
    start :: Coord2
    -- 1. Get all maze indices that are empty in Row 1
    -- 2. Sort by the column (snd)
    -- 3. Pick the first
    start = head $ {-3-}
{-2-} sortOn snd $
{-1-} filter (\c@(row, _) -> row == 1 && maze A.! c == Empty) (A.indices maze)

-- Recursively run all the moves.
-- With each call, process one element of 'directions' - turn once and move the set number of times.
runMoves :: (MonadLogger m) => MazeInfo -> (Coord2, Direction) -> [(Turn, Int)] -> m (Coord2, Direction)
runMoves _ final [] = return final {- Base Case - No more turns/moves. -}
runMoves info (currentLoc, currentDir) ((nextTurn, distance) : rest) = do
  finalCoord <- runMovesTail distance currentLoc
  runMoves info (finalCoord, newDir) rest {- Recursive -}
  where
    newDir = turn nextTurn currentDir

    -- Recursively move the given direction a set number of times.
    runMovesTail :: (MonadLogger m) => Int -> Coord2 -> m Coord2
    runMovesTail 0 c = return c {- Base Case - n=0 -}
    runMovesTail n c = do
      result <- move info (c, newDir)
      runMovesTail (n - 1) result {- Recursive Case (n - 1) -}

turn :: Turn -> Direction -> Direction
turn TurnLeft d = case d of
  FaceUp -> FaceLeft
  FaceRight -> FaceUp
  FaceDown -> FaceRight
  FaceLeft -> FaceDown
turn TurnRight d = case d of
  FaceUp -> FaceRight
  FaceRight -> FaceDown
  FaceDown -> FaceLeft
  FaceLeft -> FaceUp

-- 1. Get the next coordinate based on our direction
-- 2. If the next coordinate is empty, move there. If it's a wall, return the old location.
-- 3. If it's blank, wrap around to the next cell.
--      This requires checking the 'rowInfo' for horizontal wrapping, and the 'columnInfo' for vertical wrapping.
move :: (MonadLogger m) => MazeInfo -> (Coord2, Direction) -> m Coord2
move (maze, rowInfo, columnInfo) (loc@(row, column), direction) = return nextCell
  where
    {- 1 -}
    nextCoords = case direction of
      FaceUp -> (row - 1, column)
      FaceRight -> (row, column + 1)
      FaceDown -> (row + 1, column)
      FaceLeft -> (row, column - 1)
    nextCell = case maze A.! nextCoords of
      Wall -> loc {- 2 -}
      Empty -> nextCoords {- 2 -}
      Blank -> if maze A.! nextCellWrapped == Empty
        then nextCellWrapped
        else loc

    {- 3 -}
    nextCellWrapped = case direction of
      FaceUp -> (snd $ columnInfo A.! column, column)
      FaceRight -> (row, fst $ rowInfo A.! row)
      FaceDown -> (fst $ columnInfo A.! column, column)
      FaceLeft -> (row, snd $ rowInfo A.! row)

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution ((row, column), direction) = return $ Just (1000 * row + 4 * column + dirFactor)
  where
    dirFactor = case direction of
      FaceRight -> 0
      FaceDown -> 1
      FaceLeft -> 2
      FaceUp -> 3

-------------------- SOLVING HARD --------------------
-- What changes in Part 2 is the presence of the "Face Function" mapping each coordinate
-- to a numbered face, and the "Wrap Function" telling us where to go when we wrap around.
-- See the "Face and Wrapping Functions" section below for the hardcoded implementations of these.

type HardSolutionType = Int
type Face = Int
type MazeInfoHard = (Grid2 Cell, Coord2 -> Face)
type WrapFunction = Coord2 -> Face -> Direction -> (Coord2, Direction)

processInputHard :: (MonadLogger m) => (MazeInfoHard, [(Turn, Int)]) -> WrapFunction -> m EasySolutionType
processInputHard (mazeInfoHard@(maze, _), directions) wrap = runMovesHard mazeInfoHard wrap (start, FaceUp) directions
  where
    start = fst $ head $ sortOn (snd . fst) $ filter (\((row, _), cell) -> row == 1 && cell == Empty) (A.assocs maze)

runMovesHard :: (MonadLogger m) => MazeInfoHard -> WrapFunction -> (Coord2, Direction) -> [(Turn, Int)] -> m (Coord2, Direction)
runMovesHard _ _ final [] = return final
runMovesHard info wrap (currentLoc, currentDir) ((nextTurn, distance) : rest) = do
  (finalCoord, finalDir) <- runMovesTail distance (currentLoc, newDir)
  runMovesHard info wrap (finalCoord, finalDir) rest
  where
    newDir = turn nextTurn currentDir

    -- Unlike part 1, our direction can change without us "turning", so this function
    -- needs to return a new coordinate and a new direction.
    runMovesTail :: (MonadLogger m) => Int -> (Coord2, Direction) -> m (Coord2, Direction)
    runMovesTail 0 c = return c
    runMovesTail n (c, d) = do
      result <- moveHard info wrap (c, d)
      runMovesTail (n - 1) result

moveHard :: (MonadLogger m) => MazeInfoHard -> WrapFunction -> (Coord2, Direction) -> m (Coord2, Direction)
moveHard (maze, getFace) wrap (loc@(row, column), direction) = return result
  where
    nextCoords = case direction of
      FaceUp -> (row - 1, column)
      FaceRight -> (row, column + 1)
      FaceDown -> (row + 1, column)
      FaceLeft -> (row, column - 1)
    result = case maze A.! nextCoords of
      Wall -> (loc, direction)
      Empty -> (nextCoords, direction)
      Blank -> if maze A.! nextCellWrapped == Empty
        then (nextCellWrapped, nextDirWrapped)
        else (loc, direction)

    {- Primary difference comes with this logic, see functions below. -}
    (nextCellWrapped, nextDirWrapped) = wrap loc (getFace loc) direction

--------------- Face and Wrapping Functions ---------

-- getFace functions map
-- These are specific to the shape of the inputs (day_22_small.txt and day_22_large.txt)
getFaceEasy :: Coord2 -> Face
getFaceEasy (row, col)
  | row <= 4 = 1
  | row <= 8 && col <= 4 = 5
  | row <= 8 && col <= 8 = 3
  | row <= 8 = 2
  | col <= 12 = 6
  | otherwise = 4

getFaceHard :: Coord2 -> Face
getFaceHard (row, col)
  | row <= 50 && col <= 100 = 1
  | row <= 50 = 2
  | row <= 100 = 3
  | row <= 150 && col <= 50 = 5
  | row <= 150 = 6
  | otherwise = 4

-- NOTE: I use a different numbering for the faces than the example given in the problem description online (https://www.adventofcode.com/2022/day/22)
-- My numbering reflects how the faces connect on a standard six-sided die (observe that 1 is opposite 6, 2 to 5, and 3 to 4).
-- So for the smaller example, my numbering looks like this:
--         1111
--         1111
--         1111
--         1111
-- 555533332222
-- 555533332222
-- 555533332222
-- 555533332222
--         66664444
--         66664444
--         66664444
--         66664444

-- type WrapFunction = Coord2 -> Face -> Direction -> (Coord2, Direction)
-- Given:
--   1.) Starting Coordinate
--   2.) The face of the square we're on (1-6)
--   3.) The Direction we're facing
--
-- Find the new coordinate and direction, presuming we go off the edge.
-- These are hard-coded based on how I observed to fold the inputs (day_22_small.txt and day_22_large.txt)
wrapEasy :: WrapFunction
wrapEasy (row, col) face dir = case (face, dir) of
  (1, FaceLeft) -> ((5, row + 4), FaceDown) 
  (1, FaceUp) -> ((5, 5 - (col - 8)), FaceDown) -- Col 9-12 -> Col 4-1
  (1, FaceRight) -> ((13 - row, 16), FaceLeft) -- Top of 1 is bottom of 4
  (2, FaceRight) -> ((9, 13 + (8 - row)), FaceDown) -- Bottom of 2 is left of 4
  (3, FaceUp) -> ((col - 4, 9), FaceRight) -- Col 5-8 -> Row 1-4
  (3, FaceDown) -> ((9 + (8 - col), 9), FaceRight) -- Col 5-8 -> Row 12-9
  (4, FaceUp) -> ((5 + (16 - col), 12), FaceLeft) -- Col 13-16 -> Row 8-5
  (4, FaceRight) -> ((1 + (12 - row), 12), FaceLeft) -- Row 9-12 -> Row 4-1
  (4, FaceDown) -> ((4 + (17 - col), 1), FaceRight) -- Col 13-16 -> Row 8-5
  (5, FaceUp) -> ((1, 5 + (8 - col)), FaceDown) -- Col 1-4 -> Col 12-9
  (5, FaceLeft) -> ((12, 12 + (9 - row)), FaceUp) -- Row 5-8 -> Col 16-13
  (5, FaceDown) -> ((12, 9 + (4 - col)), FaceUp) -- Col 1-4 -> Col 12-9
  (6, FaceLeft) -> ((8, 5 + (12 - row)), FaceUp) -- Row 9-12 -> Col 8-5
  (6, FaceDown) -> ((8, 1 + (12 - col)), FaceUp) -- Col 9-12 -> Col 4-1
  _ -> error ("Invalid wrap case: " <> show (row, col, face, dir))

wrapHard :: WrapFunction
wrapHard (row, col) face dir = case (face, dir) of
  (1, FaceUp) -> ((col + 100, 1), FaceRight) -- Col 51-100 -> Row 151-200, Col 1
  (4, FaceLeft) -> ((1, row - 100), FaceDown) -- Row 151-200 -> Col 51-100, Row 1
  (1, FaceLeft) -> ((101 + (50 - row), 1), FaceRight) -- Row 1-50 -> Row 150-101, Col 1
  (5, FaceLeft) -> ((1 + (150 - row), 51), FaceRight) -- Row 101-150 -> Row 50-1, Col 51
  (2, FaceUp) -> ((200, col - 100), FaceUp) -- Col 101-150 -> Col 1-50, Row 200
  (4, FaceDown) -> ((1, col + 100), FaceDown) -- Col 1-50 -> Col 101-150 , Row 1
  (2, FaceRight) -> ((101 + (50 - row), 100), FaceLeft) -- Row 1-50 -> Row 150-101, Col 100
  (6, FaceRight) -> ((1 + (150 - row), 150), FaceLeft)   -- Row 101-150 -> Row 50-1, Col 150
  (2, FaceDown) -> ((col - 50, 100), FaceLeft) -- Col 101-150 -> Row 51-100, Col 100
  (3, FaceRight) -> ((50, row + 50), FaceUp)  -- Row 51-100 -> Col 101-150, Row 50
  (3, FaceLeft) -> ((101, row - 50), FaceDown) -- Row 51-100 -> Col 1-50, Row 101
  (5, FaceUp) -> ((col + 50, 51), FaceRight)  -- Col 1-50 -> Row 51-100, Col 51
  (4, FaceRight) -> ((150, row - 100), FaceUp) -- Row 151-200 -> Col 51-100, Row 150
  (6, FaceDown) -> ((col + 100, 50), FaceLeft) -- Col 51-100 -> Row 151-200, Col 50
  _ -> error ("Invalid wrap case: " <> show (row, col, face, dir))

-------------------- BOILERPLATE --------------------
smallFile :: FilePath
smallFile = "inputs_2022/day_" <> show dayNum <> "_small.txt"

largeFile :: FilePath
largeFile = "inputs_2022/day_" <> show dayNum <> "_large.txt"

easySmall :: IO (Maybe Int)
easySmall = solveEasy smallFile

easyLarge :: IO (Maybe Int)
easyLarge = solveEasy largeFile

hardSmall :: IO (Maybe Int)
hardSmall = solveHard "small" smallFile

hardLarge :: IO (Maybe Int)
hardLarge = solveHard "large" largeFile