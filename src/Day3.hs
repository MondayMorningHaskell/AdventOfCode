{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Text.Megaparsec (ParsecT, sepEndBy1, some, satisfy)
import Text.Megaparsec.Char (eol, char)
import Data.Void (Void)
import Data.Text (Text, pack)

import Utils (parseFile, Coord2, hashMapFromNestedLists, getNeighbors8)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as S
import Data.Char (ord, isAlphaNum, isDigit)

dayNum :: Int
dayNum = 3

-------------------- PUTTING IT TOGETHER --------------------
solveEasy :: FilePath -> IO (Maybe Int)
solveEasy fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  result <- processInputEasy input
  findEasySolution result

solveHard :: FilePath -> IO (Maybe Int)
solveHard fp = runStdoutLoggingT $ do
  input <- parseFile parseInput fp
  result <- processInputEasy input
  findHardSolution result

-------------------- PARSING --------------------
type InputType = HM.HashMap Coord2 Char

parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
parseInput = hashMapFromNestedLists <$> sepEndBy1 (some (satisfy (/= '\n'))) eol

-- parseInput :: (MonadLogger m) => ParsecT Void Text m InputType
-- parseInput =
--   sepEndyBy1 parseLine eol

-- type InputType = [LineType]
-- type LineType = ()

-- parseLine :: (MonadLogger m) => ParsecT Void Text m LineType
-- parseLine = return ()

-------------------- SOLVING EASY --------------------
type NumSpec = (Int, Coord2, Int)
type EasySolutionType = (S.Set Coord2, [NumSpec], InputType)

processInputEasy :: (MonadLogger m) => InputType -> m EasySolutionType
processInputEasy charMap = do
  numSpecs <- findNumsLoop [] emptySpec (0, 0)
  return (specialCharLocations, numSpecs, charMap)
  where
    specialCharLocations = S.fromList $ HM.keys
      (HM.filter (\c -> not (isAlphaNum c) && c /= '.') charMap)

    (maxRow, maxCol) = maximum $ HM.keys charMap

    findNumsLoop :: (MonadLogger m) => [NumSpec] -> ([Int], Coord2) -> Coord2 -> m [NumSpec]
    findNumsLoop acc (digits, startCoord) (r, c) =
      let nextChar = charMap HM.! (r, c)
          nextDigit = ord nextChar - 48
          specIsEmpty = null digits
          nextCoord = if specIsEmpty then (r,c) else startCoord
          nextSpec = buildSpec (digits, startCoord)
      in if r > maxRow
           then return acc
           else
             if c > maxCol
              then if not specIsEmpty
                then findNumsLoop (nextSpec : acc) emptySpec (r + 1, 0)
                else findNumsLoop acc emptySpec (r + 1, 0)
              else if isDigit nextChar
                then findNumsLoop acc (nextDigit : digits, nextCoord) (r, c + 1)
                else if specIsEmpty
                  then findNumsLoop acc emptySpec (r, c + 1)
                  else findNumsLoop (nextSpec : acc) emptySpec (r, c + 1)
    
    emptySpec :: ([Int], Coord2)
    emptySpec = ([], (-1,-1))

    buildSpec :: ([Int], Coord2) -> NumSpec
    buildSpec (digits, coord) =
      let f (x, t) y = (x + y * t, t * 10)
      in  (fst $ foldl f (0, 1) digits, coord, length digits)

findEasySolution :: (MonadLogger m) => EasySolutionType -> m (Maybe Int)
findEasySolution (specialLocations, numSpecs, charMap) = return $ Just (foldl processSpec 0 numSpecs)
  where
    processSpec prevSum (value, startCoord, len) = if isAdjacent (startCoord, len)
      then prevSum + value
      else prevSum
    
    isAdjacent coordAndLen = not . S.null $ S.intersection specialLocations (allNeighbors coordAndLen)

    allNeighbors ((sr, sc), n) =
      let f prevSet coord = foldl (flip S.insert) prevSet (getNeighbors8 charMap coord)
      in  foldl f S.empty [(sr, c) | c <- [sc..sc+(n-1)]]

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

processInputHard :: (MonadLogger m) => InputType -> m HardSolutionType
processInputHard = undefined


findHardSolution :: (MonadLogger m) => HardSolutionType -> m (Maybe Int)
findHardSolution (specialCharLocations, numSpecs, charMap) = return $ Just $ foldl processStar 0 starLocs
  where
    -- Find Stars
    starLocs = S.filter (\c -> charMap HM.! c == '*') specialCharLocations

    numSpecMap :: HM.HashMap Coord2 NumSpec
    numSpecMap =
      let f prevHM spec@(num, (startR, startC), len) = foldl (\hm c -> HM.insert c spec hm) prevHM [(startR,c) | c <- [startC..(startC + len - 1)]]
      in  foldl f HM.empty numSpecs

    -- For each star, find neighbors that are digits
    processStar :: Int -> Coord2 -> Int
    processStar prevSum c = case gearNeighbors c of
      Just ((n1, _, _), (n2, _, _)) -> prevSum + n1 * n2
      _ -> prevSum
    
    gearNeighbors :: Coord2 -> Maybe (NumSpec, NumSpec)
    gearNeighbors coord =
      let ns = getNeighbors8 charMap coord
          f prevSet neighbor = if HM.member neighbor numSpecMap then S.insert (numSpecMap HM.! neighbor) prevSet else prevSet
          nSet = foldl f S.empty ns
      in  if S.size nSet == 2
            then Just (S.findMin nSet, S.findMax nSet)
            else Nothing

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

easySmall :: IO (Maybe Int)
easySmall = solveEasy smallFile

easyLarge :: IO (Maybe Int)
easyLarge = solveEasy largeFile

hardSmall :: IO (Maybe Int)
hardSmall = solveHard smallFile

hardLarge :: IO (Maybe Int)
hardLarge = solveHard largeFile