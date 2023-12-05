{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Control.Monad (forM_)
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Text.Megaparsec (ParsecT, sepEndBy1, some)
import Text.Megaparsec.Char (eol, string, digitChar, char, hspace1)
import Data.Void (Void)
import Data.Text (Text, pack)
import Data.Word (Word64)
import qualified Data.Vector as V
import qualified Data.List as L

import Utils (parseFile, parseSpacedInts)

dayNum :: Int
dayNum = 5

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO Word64
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  processInputEasy input

solveHard :: FilePath -> IO Word64
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  processInputHard input

-------------------- PARSING --------------------
data InputType = InputType
  { seeds :: [Word64]
  , seedSoil :: [(Word64, Word64, Word64)]
  , soilFertilizer :: [(Word64, Word64, Word64)]
  , fertizilerWater :: [(Word64, Word64, Word64)]
  , waterLight :: [(Word64, Word64, Word64)]
  , lightTemp :: [(Word64, Word64, Word64)]
  , tempHumidity :: [(Word64, Word64, Word64)]
  , humidityLocation :: [(Word64, Word64, Word64)]
  }

-- data InputType = InputType
--   { seeds :: [Word64],
--   , seedSoil :: Int -> Int
--   , soilFertilizer :: Int -> Int
--   , fertizilerWater :: Int -> Int
--   , waterLight :: Int -> Int
--   , lightTemp :: Int -> Int
--   , tempHumidity :: Int -> Int
--   , humidityLocation :: Int -> Int
--   }

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = do
  string "seeds: "
  seeds <- fmap fromIntegral <$> parseSpacedInts
  eol
  ss <- parseIntervalMap "seed-to-soil map"
  sf <- parseIntervalMap "soil-to-fertilizer map"
  fw <- parseIntervalMap "fertilizer-to-water map"
  wl <- parseIntervalMap "water-to-light map"
  lt <- parseIntervalMap "light-to-temperature map"
  th <- parseIntervalMap "temperature-to-humidity map"
  hl <- parseIntervalMap "humidity-to-location map"
  return (InputType seeds ss sf fw wl lt th hl)

parseIntervalMap :: Text -> ParsecT Void Text m [(Word64, Word64, Word64)]
parseIntervalMap name = do
  eol
  string name
  char ':'
  eol
  sepEndBy1 parse3Ints eol
  where
    parse3Ints = do
      i1 <- read <$> some digitChar
      hspace1
      i2 <- read <$> some digitChar
      hspace1
      i3 <- read <$> some digitChar
      return (i1, i2, i3)

-- parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
-- parseInput =
--   sepEndyBy1 parseLine eol

-- type InputType = [LineType]
-- type LineType = ()

-- parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
-- parseLine = return ()

-------------------- SOLVING EASY --------------------
type EasySolutionType = Word64

mkFunction :: [(Word64, Word64, Word64)] -> (Word64 -> Word64)
mkFunction = f id
  where
    f g [] = g
    f g ((dest, start, len) : rest) =
      let h x = if x >= start && x < start + len
                  then dest + (x - start)
                  else g x
      in  f h rest

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy (InputType seedLocs ss sf fw wl lt th hl) = return $
  minimum (getLoc <$> seedLocs)
  where
    f = mkFunction
    getLoc = (f hl . f th . f lt . f wl . f fw . f sf . f ss)

findEasySolution :: (MonadLogger m) => EasySolutionType -> m Int
findEasySolution _ = return 0

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

-- Run only first
-- Debug fill ranges
-- Debug getNewRanges once
processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard (InputType seeds ss sf fw wl lt th hl) = do
  let initialRanges = pairs seeds
  return $ L.minimum (getMinStartingWithRange <$> initialRanges)
  where
    ss' = convertAndFillRange ss
    sf' = convertAndFillRange sf
    fw' = convertAndFillRange fw
    wl' = convertAndFillRange wl
    lt' = convertAndFillRange lt
    th' = convertAndFillRange th
    hl' = convertAndFillRange hl

    pairs [] = []
    pairs [_] = []
    pairs (x : y : rest) = (x, y) : pairs rest

    g = getNewRanges

    getMinStartingWithRange :: (Word64, Word64) -> Word64
    getMinStartingWithRange rng = L.minimum . fmap fst $
      [rng] >>= g ss' >>= g sf' >>= g fw' >>= g wl' >>= g lt' >>= g th' >>= g hl'

getNewRanges :: V.Vector (Word64, Word64, Word64) -> (Word64, Word64) -> [(Word64, Word64)]
getNewRanges mapIntervals (start, len) = if start >= lastS + lastL
  then [(start, len)]
  else reverse $ f startIndex [] (start, start + len - 1)
  where
    (lastD, lastS, lastL) = V.last mapIntervals
    -- Binary Search for largest source-start <= start
    startIndex = bSearch (0, V.length mapIntervals)
    bSearch (startI, endI) =
      let avg = (startI + endI) `quot` 2
          (avgD, avgS, avgL) = mapIntervals V.! avg
      in  if startI >= endI then error "Binary search failed!"
        else case (start < avgS, start < avgS + avgL) of
          (False, False) -> bSearch (avg + 1, endI)
          (False, True) -> avg
          (True, _) -> bSearch (startI, avg)

    -- Until interval is exhausted, add things starting from the dest
    -- PRECONDITION: curr is >= thisS
    f idx acc (curr, final) =
      let (thisD, thisS, thisL) = mapIntervals V.! idx
      -- Base cases:
      in  if idx >= V.length mapIntervals
            -- 1. No more intervals (add one more id)
            then reverse $ (curr, final - curr + 1) : acc
            -- 2. Current exceeds final (no more)
            else if curr > final
              then reverse acc
              -- 3. Current to final falls within the interval (one more dest mapping)
              else if final < thisS + thisL
                then reverse $ (thisD + (curr - thisS), final - curr + 1) : acc
                -- General case:
                -- Map a dest mapping from current to the end of thisL, increase current, increase idx
                else f (idx + 1) ((thisD + (curr - thisS), thisL - (curr - thisS)) : acc) (thisS + thisL, final)

view2 :: (a, b, c) -> b
view2 (_, y, _) = y

-- [(50,98,2), (52,50,48)]
-- [(0,0,50),(52,50,48),(50,98,2)]
convertAndFillRange :: [(Word64, Word64, Word64)] -> V.Vector (Word64, Word64, Word64)
convertAndFillRange intervals = V.fromList (fill 0 [] sortedIntervals)
  where
    sortedIntervals = L.sortOn view2 intervals

    fill prevEnd acc [] = reverse acc
    fill prevEnd acc ints@((nextD, nextS, nextL) : rest) = if prevEnd < nextS
      then fill nextS ((prevEnd, prevEnd, nextS - prevEnd) : acc) ints
      else fill (nextS + nextL) ((nextD, nextS, nextL) : acc) rest

findHardSolution :: (MonadLogger m) => HardSolutionType -> m Int
findHardSolution _ = return 0

-------------------- SOLUTION PATTERNS --------------------

-- solveFold :: (MonadLogger m) => [LineType] -> m EasySolutionType
-- solveFold = foldM foldLine initialFoldV

-- type FoldType = ()

-- initialFoldV :: FoldType
-- initialFoldV = undefined

-- foldLine :: (MonadLogger m) => FoldType -> LineType -> m FoldType
-- foldLine = undefined

-- type StateType = ()

-- initialStateV :: StateType
-- initialStateV = ()

-- solveStateN :: (MonadLogger m) => Int -> StateType -> m StateType
-- solveStateN 0 st = return st
-- solveStateN n st = do
--   st' <- evolveState st
--   solveStateN (n - 1) st'

-- evolveState :: (MonadLogger m) => StateType -> m StateType
-- evolveState st = undefined

-------------------- BOILERPLATE --------------------
smallFile :: FilePath
smallFile = "inputs_2022/day_" <> show dayNum <> "_small.txt"

largeFile :: FilePath
largeFile = "inputs_2022/day_" <> show dayNum <> "_large.txt"

easySmall :: IO Word64
easySmall = solveEasy smallFile

easyLarge :: IO Word64
easyLarge = solveEasy largeFile

hardSmall :: IO Word64
hardSmall = solveHard smallFile

hardLarge :: IO Word64
hardLarge = solveHard largeFile