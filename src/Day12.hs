{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Data.Char (isLower)
import Data.List (all)
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Text.Megaparsec (ParsecT, choice, some)
import Text.Megaparsec.Char (string, char, letterChar)
import Data.Void (Void)
import Data.Text (Text, unpack, pack)
import Utils (parseLinesFromFile, OccMap, incKey, occLookup, emptyOcc)

d12ES :: IO (Maybe Int)
d12ES = solveDay12Easy "inputs/day_12_small.txt"

d12EB :: IO (Maybe Int)
d12EB = solveDay12Easy "inputs/day_12_big.txt"

d12HS :: IO (Maybe Int)
d12HS = solveDay12Hard "inputs/day_12_basic_2.txt"

d12HB :: IO (Maybe Int)
d12HB = solveDay12Hard "inputs/day_12_big.txt"

solveDay12Easy :: String -> IO (Maybe Int)
solveDay12Easy fp = do
  graphLines <- parseLinesFromFile parseGraphLine fp
  let graph = constructGraph graphLines
  result <- runStdoutLoggingT $ countPaths' easyVisitedPred graph (HS.empty, False) "start"
  return $ Just result

solveDay12Hard :: String -> IO (Maybe Int)
solveDay12Hard fp = do
  graphLines <- parseLinesFromFile parseGraphLine fp
  let graph = constructGraph graphLines
  result <- runStdoutLoggingT $ countPaths' hardVisitedPred graph (HS.empty, False) "start"
  return $ Just result

newtype Graph = Graph
  { graphEdges :: HashMap String (HashSet String) }
  deriving (Show)

parseGraphLine :: (Monad m) => ParsecT Void Text m (String, String)
parseGraphLine = do
  node1 <- parseNodeName
  char '-'
  node2 <- parseNodeName
  return (unpack node1, unpack node2)
  where
    parseNodeName :: ParsecT Void Text m Text
    parseNodeName = choice [string "start", string "end", pack <$> some letterChar] --  , string "end", some letterChar]

constructGraph :: [(String, String)] -> Graph
constructGraph inputs = Graph (foldl f HM.empty inputs)
  where
    f :: HashMap String (HashSet String) -> (String, String) -> HashMap String (HashSet String)
    f prevMap (src, dst) =
      let prevEdges1 = fromMaybe HS.empty (HM.lookup src prevMap)
          prevEdges2 = fromMaybe HS.empty (HM.lookup dst prevMap)
          map2 = HM.insert src (HS.insert dst prevEdges1) prevMap
      in  HM.insert dst (HS.insert src prevEdges2) map2

-- Graph is not directed
countPaths' :: (MonadLogger m) =>
  ((HashSet String, Bool)-> String -> Bool) -> Graph -> (HashSet String, Bool) -> String -> m Int
countPaths' visitedPred gr@(Graph edges) v@(visited, hasSeenSmall) top = do
  if top == "end"
    then return 1
    else
      do
        let newEdges = HS.filter (not . visitedPred v) $ fromMaybe HS.empty (HM.lookup top edges)
            newVisited = if HS.member top visited && isSmall top
              then (visited, True)
              else (HS.insert top visited, hasSeenSmall)
        logDebugN $ "Processing Top: " <> pack top
        logDebugN $ "New Edges: " <> (pack . show $ newEdges)
        logDebugN $ "Old Visited : " <> (pack . show $ v)
        logDebugN $ "New Visited : " <> (pack . show $ newVisited)
        sum <$> mapM (countPaths' visitedPred gr newVisited) (HS.toList newEdges)

easyVisitedPred :: (HashSet String, Bool) -> String -> Bool
easyVisitedPred _ "start" = True
easyVisitedPred (visited, _) node = isSmall node && HS.member node visited

hardVisitedPred :: (HashSet String, Bool) -> String -> Bool
hardVisitedPred _ "start" = True
hardVisitedPred (visited, hasSeenSmall) node = isSmall node && HS.member node visited && hasSeenSmall

isSmall :: String -> Bool
isSmall = all isLower
