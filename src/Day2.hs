module Day2 where

import Text.Megaparsec

d2ES :: IO Int
d2ES = solveDay2Easy "inputs/day_2_small.txt"

d2EB :: IO Int
d2EB = solveDay2Easy "inputs/day_2_big.txt"

d2HS :: IO Int
d2HS = solveDay2Hard "inputs/day_2_small.txt"

d2HB :: IO Int
d2HB = solveDay2Hard "inputs/day_2_big.txt"

solveDay2Easy :: String -> IO Int
solveDay2Easy fp = return 0

solveDay2Hard :: String -> IO Int
solveDay2Hard fp = return 0

data Instruction =
    Forward Int |
    Down Int |
    Up Int
    deriving (Show, Eq)

{-
parseInstruction :: String -> Instruction
parseInstruction _ = case runParser myParser of
    Left _ -> error "Could not parse!"
    Right x -> x
  where
    myParser =
        (try (string "Foward") >> ) <|>
        (try (string "Down") >> ) <|>
        (try (string "Up") >> )
-}