{-# LANGUAGE OverloadedStrings #-}

module Day24 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logErrorN)
import Text.Megaparsec (ParsecT, sepEndBy1, (<|>), some)
import Text.Megaparsec.Char (eol, char)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, parse2DHashMap, Coord2, getNeighbors4, manhattanDistance)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Array as A
import qualified Data.HashSet as HS
import Algorithm.Search (bfsM, aStarM)
import Control.Monad (filterM, when)
import Data.Bits
import Data.List.Extra (groupOn)
import Control.Monad.Cont (lift)

dayNum :: Int
dayNum = 24

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO (Maybe Int)
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  Just <$> processInputEasy input

solveHard :: FilePath -> IO (Maybe Int)
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  Just <$> processInputHard input

-------------------- PARSING --------------------
type InputType = (Coord2, Coord2, BlizzardMap', HM.HashMap Coord2 Cell)

data BlizzardMap = BlizzardMap
  { upBlizzards :: HS.HashSet Coord2
  , downBlizzards :: HS.HashSet Coord2
  , rightBlizzards :: HS.HashSet Coord2
  , leftBlizzards :: HS.HashSet Coord2
  } deriving (Show, Eq, Ord)

data Cell =
  Empty |
  Wall |
  BlizzUp |
  BlizzDown |
  BlizzRight |
  BlizzLeft
  deriving (Show, Eq, Ord)

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = do
  cells <- parse2DHashMap (some parseCell)
  let emptySpaces = filter (\(_, c) -> c == Empty) (HM.toList cells)
  let emptyCoords = sortOn fst (fst <$> emptySpaces)
  let start = head emptyCoords
  let end = last emptyCoords
  bm <- lift $ mkBlizzardMap' cells
  return (start, end, bm, cells)

parseCell :: ParsecT Void Text m Cell
parseCell =
  (char '.' >> return Empty) <|>
  (char '#' >> return Wall) <|>
  (char '^' >> return BlizzUp) <|>
  (char 'v' >> return BlizzDown) <|>
  (char '>' >> return BlizzRight) <|>
  (char '<' >> return BlizzLeft)

mkBlizzardMap :: HM.HashMap Coord2 Cell -> BlizzardMap
mkBlizzardMap cells = BlizzardMap upCells downCells rightCells leftCells
  where
    assocs = HM.toList cells
    upCells = HS.fromList (fst <$> filter ((== BlizzUp) . snd) assocs)
    downCells = HS.fromList (fst <$> filter ((== BlizzDown) . snd) assocs)
    rightCells = HS.fromList (fst <$> filter ((== BlizzRight) . snd) assocs)
    leftCells = HS.fromList (fst <$> filter ((== BlizzLeft) . snd) assocs)

mkBlizzardMap' :: (MonadLogger m) => HM.HashMap Coord2 Cell -> m BlizzardMap'
mkBlizzardMap' cells = do
  return $ BlizzardMap' upArray downArray rightArray leftArray
  where
    assocs = HM.toList cells

    upCells = fst <$> filter ((== BlizzUp) . snd) assocs
    initialUpMap = HM.fromList [(c,0 :: Integer) | c <- [1..(maxCol - 1)]]
    upCellMap = foldl (\prevMap (r, c) -> HM.insert c (setBit (prevMap HM.! c) (r - 1)) prevMap) initialUpMap upCells
    upArray = A.array (1, maxCol - 1) $ HM.toList upCellMap

    downCells = fst <$> filter ((== BlizzDown) . snd) assocs
    initialDownMap = HM.fromList [(c,0 :: Integer) | c <- [1..(maxCol - 1)]]
    downCellMap = foldl (\prevMap (r, c) -> HM.insert c (setBit (prevMap HM.! c) (r - 1)) prevMap) initialDownMap downCells
    downArray = A.array (1, maxCol - 1) $ HM.toList downCellMap

    rightCells = fst <$> filter ((== BlizzRight) . snd) assocs
    initialRightMap = HM.fromList [(r, 0 :: Integer) | r <- [1..(maxRow - 1)]]
    rightCellMap = foldl (\prevMap (r, c) -> HM.insert r (setBit (prevMap HM.! r) (c - 1)) prevMap) initialRightMap rightCells
    rightArray = A.array (1, maxRow - 1) $ HM.toList rightCellMap

    leftCells = fst <$> filter ((== BlizzLeft) . snd) assocs
    initialLeftMap = HM.fromList [(r, 0 :: Integer) | r <- [1..(maxRow - 1)]]
    leftCellMap = foldl (\prevMap (r, c) -> HM.insert r (setBit (prevMap HM.! r) (c - 1)) prevMap) initialLeftMap leftCells
    leftArray = A.array (1, maxRow - 1) $ HM.toList leftCellMap

    (maxRow, maxCol) = maximum $ HM.keys cells

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy (start, end, blizzardMap, cells) = do
  -- result <- bfsM (neighbors cells minCoord maxCoord) (\st -> return (currentLocation st == end)) initialState
  result <- aStarM (neighbors cells minCoord maxCoord) (\_ _ -> return 1) (estimateCost end) (\st -> return (currentLocation st == end)) initialState
  case result of
    Nothing -> return maxBound
    Just (_, path) -> return $ length path
  where
    initialState = SearchState blizzardMap 0 start
    minCoord = minimum $ HM.keys cells
    maxCoord = maximum $ HM.keys cells

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution _ = return Nothing

data SearchState = SearchState
  { blizzMap :: BlizzardMap'
  , time :: Int
  , currentLocation :: Coord2
  } deriving (Show, Eq, Ord)

neighbors :: (MonadLogger m) => HM.HashMap Coord2 Cell -> Coord2 -> Coord2 -> SearchState -> m [SearchState]
neighbors cells minCoord maxCoord (SearchState blizzardMap t loc) = do
  nextBlizzards <- updateBlizzardMap' minCoord maxCoord blizzardMap
  logErrorN ("Time " <> (pack . show $ t))
  finalLocs <- filterM (hasNoBlizzard' (fst maxCoord) nextBlizzards) (loc : notWalls)
  return (SearchState nextBlizzards (t + 1) <$> finalLocs)
  where
    startingLocs = getNeighbors4 cells loc
    notWalls = filter (\c -> cells HM.! c /= Wall) startingLocs

hasBlizzard :: (MonadLogger m) => BlizzardMap -> Coord2 -> m Bool
hasBlizzard (BlizzardMap up down right left) c = return $ not $ or
  [HS.member c up, HS.member c down, HS.member c right, HS.member c left]

updateBlizzardMap :: (MonadLogger m) => Coord2 -> Coord2 -> BlizzardMap -> m BlizzardMap
updateBlizzardMap (minRow, minCol) (maxRow, maxCol) (BlizzardMap up down right left) = return $ BlizzardMap
  (HS.map up' up)
  (HS.map down' down)
  (HS.map right' right)
  (HS.map left' left)
  where
    up' (r, c) = (if r - 1 == minRow then maxRow - 1 else r - 1, c)
    down' (r, c) = (if r + 1 == maxRow then minRow + 1 else r + 1, c)
    right' (r, c) = (r, if c + 1 == maxCol then minCol + 1 else c + 1)
    left' (r, c) = (r, if c - 1 == minCol then maxCol - 1 else c - 1)

estimateCost :: (MonadLogger m) => Coord2 -> SearchState -> m Int
estimateCost end (SearchState _ _ loc) = return (manhattanDistance loc end)

-- For efficiency, store each row of the Blizzard as a BitMap
-- The 0 bit refers to the left row or the top most column.
-- So to move the "up blizzards", we do a "right" bit shift.
-- To move "down blizzards" we do a "left" bit shift.
-- To move "left blizzards" we unintuitively do a "right" bit shift.
-- And to move "right blizzards" we do a "left" bit shift.
-- All these shifts must account for rotating the final bit around.
-- Also, this introduces off-by-one issues we have to deal with.
-- To test if (row, col) contains an "up" blizzard, we index into "col"
-- but then test the bit "row - 1" because Row 1 refers to the 0 bit.
-- To test if (row, col) contains a "right" blizzard, we index into "row"
-- and then check "col - 1"
data BlizzardMap' = BlizzardMap'
  { upBlizzards' :: A.Array Int Integer
  , downBlizzards' :: A.Array Int Integer
  , rightBlizzards' :: A.Array Int Integer
  , leftBlizzards' :: A.Array Int Integer
  } deriving (Show, Eq, Ord)

hasNoBlizzard' :: (MonadLogger m) => Int -> BlizzardMap' -> Coord2 -> m Bool
hasNoBlizzard' maxRow (BlizzardMap' up down right left) (row, col) = if row == 0 || row == maxRow then return True
  else return $ not $ or
    [isUp, isDown, isRight, isLeft]
  where
    isUp = testBit (up A.! col) (row - 1)
    isDown = testBit (down A.! col) (row - 1)
    isRight = testBit (right A.! row) (col - 1)
    isLeft = testBit (left A.! row) (col - 1)

updateBlizzardMap' :: (MonadLogger m) => Coord2 -> Coord2 -> BlizzardMap' -> m BlizzardMap'
updateBlizzardMap' (minRow, minCol) (maxRow, maxCol) (BlizzardMap' up down right left) = return $ BlizzardMap'
  up' down' right' left'
  where
    up' = fmap (shiftIRight (maxRow - 2)) up
    down' = fmap (shiftILeft (maxRow - 2)) down
    right' = fmap (shiftILeft (maxCol - 2)) right
    left' = fmap (shiftIRight (maxCol - 2)) left

shiftILeft :: Int -> Integer -> Integer
shiftILeft maxBit i = shiftL base 1 + if maxIs1 then 1 else 0
  where
    maxIs1 = testBit i maxBit
    base = clearBit i maxBit

shiftIRight :: Int -> Integer -> Integer
shiftIRight maxBit i = if minIs1
  then setBit base maxBit
  else base
  where
    minIs1 = testBit i 0
    base = shiftR i 1

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard (start, end, blizzardMap, cells) = do
  result <- aStarM (neighbors cells minCoord maxCoord) (\_ _ -> return 1) (estimateCost end) (\st -> return (currentLocation st == end)) initialState
  case result of
    Nothing -> return maxBound
    Just (_, path) -> do
      result2 <- aStarM (neighbors cells minCoord maxCoord) (\_ _ -> return 1) (estimateCost start) (\st -> return (currentLocation st == start)) (last path)
      case result2 of
        Nothing -> logErrorN "Couldn't make it back!" >> return maxBound
        Just (_, path2) -> do
          result3 <- aStarM (neighbors cells minCoord maxCoord) (\_ _ -> return 1) (estimateCost end) (\st -> return (currentLocation st == end)) (last path2)
          case result3 of
            Nothing -> logErrorN "Couldn't make the final leg!" >> return maxBound
            Just (_, path3) -> do
              return $ length path + length path2 + length path3
  where
    initialState = SearchState blizzardMap 0 start
    minCoord = minimum $ HM.keys cells
    maxCoord = maximum $ HM.keys cells

findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution _ = return Nothing

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
hardSmall = solveHard smallFile

hardLarge :: IO (Maybe Int)
hardLarge = solveHard largeFile