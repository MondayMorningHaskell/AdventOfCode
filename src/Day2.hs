{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Utils (parsePositiveNumber, readLinesFromFile)

d2ES :: IO Int
d2ES = solveDay2Easy "inputs/day_2_small.txt"

d2EB :: IO Int
d2EB = solveDay2Easy "inputs/day_2_big.txt"

d2HS :: IO Int
d2HS = solveDay2Hard "inputs/day_2_small.txt"

d2HB :: IO Int
d2HB = solveDay2Hard "inputs/day_2_big.txt"

solveDay2Easy :: String -> IO Int
solveDay2Easy fp = instructionsToSolutionEasy <$> readLinesFromFile parseInstruction fp

solveDay2Hard :: String -> IO Int
solveDay2Hard fp = instructionsToSolutionHard <$> readLinesFromFile parseInstruction fp

data Instruction =
    Forward Int |
    Down Int |
    Up Int
    deriving (Show, Eq)

parseInstruction :: String -> Instruction
parseInstruction input = case runParser myParser "Day2.hs" (pack input) of
    Left e -> error $ "Could not parse: " ++ show e
    Right x -> x
  where
    myParser :: Parsec Void Text Instruction
    myParser = do
      constructor <-
        try (string "forward " >> return Forward) <|>
        try (string "up " >> return Up) <|>
        (string "down " >> return Down)
      constructor <$> parsePositiveNumber

instructionsToSolutionEasy :: [Instruction] -> Int
instructionsToSolutionEasy instructions = finalHorizontal * finalVertical
  where
    (finalHorizontal, finalVertical) = foldl f (0, 0) instructions

    f :: (Int, Int) -> Instruction -> (Int, Int)
    f (h0, v0) instruction = case instruction of
      Forward x -> (h0 + x, v0)
      Up x -> (h0, v0 - x)
      Down x -> (h0, v0 + x)

instructionsToSolutionHard :: [Instruction] -> Int
instructionsToSolutionHard instructions = finalHorizontal * finalVertical
  where
    (finalHorizontal, finalVertical, _) = foldl f (0, 0, 0) instructions

    f :: (Int, Int, Int) -> Instruction -> (Int, Int, Int)
    f (h0, v0, aim0) instruction = case instruction of
      Forward x -> (h0 + x, v0 + (aim0 * x), aim0)
      Up x -> (h0, v0, aim0 - x)
      Down x -> (h0, v0, aim0 + x)
