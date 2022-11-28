{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day5 where

import Text.Megaparsec (ParsecT)
import Data.Void
import Data.Text (Text, pack)
import Text.Megaparsec.Char (char, string, eol)
import Utils (parsePositiveNumber, parseLinesFromFile, Coord2, OccMap, emptyOcc, incKey, countWhere)
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT)
import qualified Data.Map as M
import Control.Monad (forM_)

d5ES :: IO (Maybe Int)
d5ES = solveDay5Easy "inputs/day_5_small.txt"

d5EB :: IO (Maybe Int)
d5EB = solveDay5Easy "inputs/day_5_big.txt"

d5HS :: IO (Maybe Int)
d5HS = solveDay5Hard "inputs/day_5_small.txt"

d5HB :: IO (Maybe Int)
d5HB = solveDay5Hard "inputs/day_5_big.txt"

type Vent = (Coord2, Coord2)

solveDay5Easy :: String -> IO (Maybe Int)
solveDay5Easy fp = do
  inputs <- parseLinesFromFile parseTupleLine fp
  runStdoutLoggingT $ solveEasy inputs

solveDay5Hard :: String -> IO (Maybe Int)
solveDay5Hard fp = do
  inputs <- parseLinesFromFile parseTupleLine fp
  runStdoutLoggingT $ solveHard inputs

parseTupleLine :: (Monad m) => ParsecT Void Text m Vent
parseTupleLine = do
  x1 <- parsePositiveNumber
  char ','
  x2 <- parsePositiveNumber
  string " -> "
  y1 <- parsePositiveNumber
  char ','
  y2 <- parsePositiveNumber
  return ((x1, x2), (y1, y2))

solveEasy :: (MonadLogger m) => [Vent] -> m (Maybe Int)
solveEasy vents = do
  ventMap <- buildVentMap (filter isHV vents)
  Just <$> countDanger ventMap
  where
    isHV :: Vent -> Bool
    isHV v = isHorizontalVent v || isVerticalVent v

solveHard :: (MonadLogger m) => [Vent] -> m (Maybe Int)
solveHard vents = do
  ventMap <- buildVentMap vents
  Just <$> countDanger ventMap

isVerticalVent :: Vent -> Bool
isVerticalVent ((x1, y1), (x2, y2)) = x1 == x2

isHorizontalVent :: Vent -> Bool
isHorizontalVent ((x1, y1), (x2, y2)) = y1 == y2

coordsInVent :: Vent -> [Coord2]
coordsInVent v@((x1, y1), (x2, y2)) 
  | isVerticalVent v = map (x1,) [(min y1 y2)..(max y1 y2)]
  | isHorizontalVent v = map (,y1) [(min x1 x2)..(max x1 x2)]
  | otherwise = 
      let xsBase = [(min x1 x2)..(max x1 x2)]
          ysBase = [(min y1 y2)..(max y1 y2)]
          xs = if x1 < x2 then xsBase else reverse xsBase
          ys = if y1 < y2 then ysBase else reverse ysBase
      in  zip xs ys

buildVentMap :: (MonadLogger m) => [Vent] -> m (OccMap Coord2)
buildVentMap vents = return $ foldl foldVent emptyOcc vents
  where
    foldVent :: OccMap Coord2 -> Vent -> OccMap Coord2
    foldVent prevMap v = foldl
      incKey
      prevMap
      (coordsInVent v)

countDanger :: (MonadLogger m) => OccMap Coord2 -> m Int
countDanger ventMap = return $ countWhere (>= 2) (M.elems ventMap)