{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import Text.Megaparsec ()
import Text.Megaparsec.Char ()
import Data.Either (partitionEithers)
import Data.List (sort)
import Data.Void (Void)
import Data.Text (Text)
import Utils (readStringsFromFile)
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)

d10ES :: IO (Maybe Integer)
d10ES = solveDay10Easy "inputs/day_10_small.txt"

d10EB :: IO (Maybe Integer)
d10EB = solveDay10Easy "inputs/day_10_big.txt"

d10HS :: IO (Maybe Integer)
d10HS = solveDay10Hard "inputs/day_10_small.txt"

d10HB :: IO (Maybe Integer)
d10HB = solveDay10Hard "inputs/day_10_big.txt"

solveDay10Easy :: String -> IO (Maybe Integer)
solveDay10Easy fp = do
  inputs <- readStringsFromFile fp
  (corruptedValues, _) <- runStdoutLoggingT $ do
    results <- mapM processForCorruption inputs
    return $ partitionEithers results
  return $ Just (sum corruptedValues)

solveDay10Hard :: String -> IO (Maybe Integer)
solveDay10Hard fp = do
  inputs <- readStringsFromFile fp
  (_, remainingStacks) <- runStdoutLoggingT $ do
    results <- mapM processForCorruption inputs
    return $ partitionEithers results
  let finalScores = sort $ scoreStack <$> remainingStacks
  return $ Just (finalScores !! (length finalScores `quot` 2))

scoreStack :: [Char] -> Integer
scoreStack = foldl f 0
  where
    f :: Integer -> Char -> Integer
    f prevScore c = 5 * prevScore + scoreChar2 c

processForCorruption :: MonadLogger m => String -> m (Either Integer [Char])
processForCorruption input = processTail input []
  where
    -- processTail :: MonadLogger m => String -> [Char] -> m (Either Integer String)
    processTail [] stack = return $ Right stack
    processTail (topChar : rest) stack = case topChar of
      '(' -> processTail rest (')' : stack)
      '[' -> processTail rest (']' : stack)
      '{' -> processTail rest ('}' : stack)
      '<' -> processTail rest ('>' : stack)
      other -> if null stack || head stack /= other
        then return (Left . scoreChar $ other)
        else processTail rest (tail stack)

scoreChar :: Char -> Integer
scoreChar ')' = 3
scoreChar ']' = 57
scoreChar '}' = 1197
scoreChar '>' = 25137
scoreChar _ = 0

scoreChar2 :: Char -> Integer
scoreChar2 ')' = 1
scoreChar2 ']' = 2
scoreChar2 '}' = 3
scoreChar2 '>' = 4
scoreChar2 _ = 0
