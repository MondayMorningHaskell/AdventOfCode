{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import Text.Megaparsec (ParsecT, some, sepEndBy1)
import Text.Megaparsec.Char (letterChar, eol, string)
import Data.Void (Void)
import Data.Text (Text, pack)
import Utils (parseFile, incKey, emptyOcc, OccMapBig, addKey)
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN, logErrorN)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Monad (foldM)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

d14ES :: IO (Maybe Integer)
d14ES = solveDay14Easy "inputs/day_14_small.txt"

d14EB :: IO (Maybe Integer)
d14EB = solveDay14Easy "inputs/day_14_big.txt"

d14HS :: IO (Maybe Integer)
d14HS = solveDay14Hard "inputs/day_14_small.txt"

d14HB :: IO (Maybe Integer)
d14HB = solveDay14Hard "inputs/day_14_big.txt"

solveDay14Easy :: String -> IO (Maybe Integer)
solveDay14Easy fp = runStdoutLoggingT $ do
  (starterCode, pairCodes) <- parseFile parseInput fp
  expandPolymerLong 10 starterCode pairCodes

solveDay14Hard :: String -> IO (Maybe Integer)
solveDay14Hard fp = runStdoutLoggingT $ do
  (starterCode, pairCodes) <- parseFile parseInput fp
  expandPolymerLong 40 starterCode pairCodes

type PairMap = HashMap (Char, Char) Char

-- Doesn't work well beyond size 10 or so
expandPolymerNaive :: (MonadLogger m) => Int -> String -> PairMap -> m (Maybe Integer)
expandPolymerNaive numSteps starterCode pairMap = do
  finalString <- foldM runStep starterCode [1..numSteps]
  let charMap = M.elems $ foldl incKey emptyOcc finalString
  if null charMap
    then logErrorN "Final Occurrence Map is empty!" >> return Nothing
    else return $ Just $ fromIntegral (maximum charMap - minimum charMap)
  where
    runStep :: (MonadLogger m) => String -> Int -> m String
    runStep input _ = runExpand "" input

    runExpand :: (MonadLogger m) => String -> String -> m String
    runExpand accum "" = logErrorN "Reached empty remainder!" >> return (reverse accum) -- < This case shouldn't happen
    runExpand accum [lastChar] = return $ reverse (lastChar : accum)
    runExpand accum (nextChar : secondChar : rest) = case HM.lookup (nextChar, secondChar) pairMap of
        Nothing -> logErrorN ("Missing Case: " <> pack [nextChar, secondChar]) >> runExpand (nextChar : accum) (secondChar : rest)
        Just insertChar -> runExpand (insertChar : nextChar : accum) (secondChar : rest)

expandPolymerLong :: (MonadLogger m) => Int -> String -> PairMap -> m (Maybe Integer)
expandPolymerLong numSteps starterCode pairMap = do
  let starterMap = buildMapF M.empty starterCode
  finalOccMap <- foldM runStep starterMap [1..numSteps]
  let finalCharCountMap = foldl countChars M.empty (M.toList finalOccMap)
  let finalCounts = map quotRoundUp (M.toList finalCharCountMap)
  if null finalCounts
    then logErrorN "Final Occurrence Map is empty!" >> return Nothing
    else return $ Just $ fromIntegral (maximum finalCounts - minimum finalCounts)
  where
    buildMapF :: OccMapBig (Char, Char) -> String -> OccMapBig (Char, Char)
    buildMapF prevMap "" = prevMap
    buildMapF prevMap [_] = prevMap
    buildMapF prevMap (firstChar : secondChar : rest) = buildMapF (incKey prevMap (firstChar, secondChar)) (secondChar : rest)

    runStep :: (MonadLogger m) => OccMapBig (Char, Char) -> Int -> m (OccMapBig (Char, Char))
    runStep prevMap _ = foldM runExpand M.empty (M.toList prevMap)

    runExpand :: (MonadLogger m) => OccMapBig (Char, Char) -> ((Char, Char), Integer) -> m (OccMapBig (Char, Char))
    runExpand prevMap (code@(c1, c2), count) = case HM.lookup code pairMap of
      Nothing -> logErrorN ("Missing Code: " <> pack [c1, c2]) >> return prevMap
      Just newChar -> do
        let first = (c1, newChar)
            second = (newChar, c2)
        return $ addKey (addKey prevMap first count) second count
  
    countChars :: OccMapBig Char -> ((Char, Char), Integer) -> OccMapBig Char
    countChars prevMap ((c1, c2), count) = addKey (addKey prevMap c1 count) c2 count

    quotRoundUp :: (Char, Integer) -> Integer
    quotRoundUp (c, i) = if even i
      then quot i 2 + if head starterCode == c && last starterCode == c
        then 1
        else 0
      else quot i 2 + 1

buildPairMap :: [(Char, Char, Char)] -> HashMap (Char, Char) Char
buildPairMap = foldl (\prevMap (c1, c2, c3) -> HM.insert (c1, c2) c3 prevMap) HM.empty

parseInput :: (MonadLogger m) => ParsecT Void Text m (String, PairMap)
parseInput = do
  starterCode <- some letterChar
  eol >> eol
  pairCodes <- sepEndBy1 parsePairCode eol
  return (starterCode, buildPairMap pairCodes)

parsePairCode :: (MonadLogger m) => ParsecT Void Text m (Char, Char, Char)
parsePairCode = do
  input1 <- letterChar
  input2 <- letterChar
  string " -> "
  output <- letterChar
  return (input1, input2, output)