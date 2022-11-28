module Monad where

import Data.Char
import Data.List.Split (splitOn)

solveMonad' :: FilePath -> IO ()
solveMonad' fp = do
  instrs <- (map parseInstruction . lines) <$> readFile fp
  let finalState = foldl foldInstruction rs0 instrs
  print $ finalState

data Exp =
  Var Char | Const Integer
  deriving (Show, Eq)

data Instruction =
  Input Char |
  Add Char Exp |
  Mul Char Exp |
  Div Char Exp |
  Mod Char Exp |
  Eql Char Exp
  deriving (Show, Eq)

parseInstruction :: String -> Instruction
parseInstruction input = case splitOn " " input of
  [command, reg] -> if command == "inp"
    then Input (head reg)
    else error $ "Invalid command: " ++ input
  [command, reg, expr] -> case command of
    "add" -> Add (head reg) (parseExpr expr)
    "mul" -> Mul (head reg) (parseExpr expr)
    "div" -> Div (head reg) (parseExpr expr)
    "mod" -> Mod (head reg) (parseExpr expr)
    "eql" -> Eql (head reg) (parseExpr expr)
    _ -> error $ "Invalid command: " ++ input
  _ -> error $ "Invalid command: " ++ input

parseExpr :: String -> Exp
parseExpr expr = if length expr == 1 && isLetter (head expr)
  then Var (head expr)
  else Const (read expr)

-- Register Expression, NOT Regular Expression
data RegExp =
  InputExp Input |
  ConstExp Integer |
  AddExp RegExp RegExp |
  MulExp RegExp RegExp |
  DivExp RegExp RegExp |
  ModExp RegExp RegExp |
  EqlExp RegExp RegExp
  deriving (Show, Eq)

data RegState = RegState
  { wReg :: RegExp
  , xReg :: RegExp
  , yReg :: RegExp
  , zReg :: RegExp
  , remainingInputs :: [Input]
  } deriving (Show, Eq)

rs0 :: RegState
rs0 = RegState (ConstExp 0) (ConstExp 0) (ConstExp 0) (ConstExp 0)
  [ ConstInput 1
  , ConstInput 1
  , ConstInput 1
  , ConstInput 1
  , ConstInput 8
  , ConstInput 1
  , ConstInput 5
  , ConstInput 1
  , ConstInput 6
  , ConstInput 3
  , ConstInput 7
  , ConstInput 1
  , ConstInput 1
  , ConstInput 2
  ]
  -- [I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14]

data Input = I1 | I2 | I3 | I4 | I5 | I6 | I7 | I8 | I9 | I10 | I11 | I12 | I13 | I14
  | ConstInput Integer
  deriving (Show, Eq)

foldInstruction :: RegState -> Instruction -> RegState
foldInstruction regState instr = case instr of
  Input regChar -> updateRegisterForInput regChar regState
  Add regChar exp -> updateRegisterForAdd regChar (expToRegExp exp regState) regState
  Mul regChar exp -> updateRegisterForMul regChar (expToRegExp exp regState) regState
  Div regChar exp -> updateRegisterForDiv regChar (expToRegExp exp regState) regState
  Mod regChar exp -> updateRegisterForMod regChar (expToRegExp exp regState) regState
  Eql regChar exp -> updateRegisterForEql regChar (expToRegExp exp regState) regState
  
rsLens :: Char -> (RegState -> RegExp)
rsLens 'w' = wReg
rsLens 'x' = xReg
rsLens 'y' = yReg
rsLens 'z' = zReg
rsLens c = error $ "Invalid register " ++ [c]

updateRegisterForInput :: Char -> RegState -> RegState
updateRegisterForInput c regState = case c of
  'w' -> regState { wReg = finalInput, remainingInputs = restInputs }
  'x' -> regState { xReg = finalInput, remainingInputs = restInputs }
  'y' -> regState { yReg = finalInput, remainingInputs = restInputs }
  'z' -> regState { zReg = finalInput, remainingInputs = restInputs }
  _ -> error $ "Invalid register " ++ [c]
  where
    (nextInput, restInputs) = (head (remainingInputs regState), tail (remainingInputs regState))
    finalInput = case nextInput of
      ConstInput a -> ConstExp a
      b -> InputExp b

updateRegisterForAdd :: Char -> RegExp -> RegState -> RegState
updateRegisterForAdd c exp regState = newState
  where
    currentExp = (rsLens c) regState
    result = case (currentExp, exp) of
      (ConstExp a, ConstExp b) -> ConstExp (a + b)
      (e1, ConstExp 0) -> e1
      (ConstExp 0, e2) -> e2
      (e1, e2) -> AddExp e1 e2
    newState = case c of
      'w' -> regState { wReg = result }
      'x' -> regState { xReg = result }
      'y' -> regState { yReg = result }
      'z' -> regState { zReg = result }

updateRegisterForMul :: Char -> RegExp -> RegState -> RegState
updateRegisterForMul c exp regState = newState
  where
    currentExp = (rsLens c) regState
    result = case (currentExp, exp) of
      (ConstExp a, ConstExp b) -> ConstExp (a * b)
      (_, ConstExp 0) -> ConstExp 0
      (e1, ConstExp 1) -> e1
      (ConstExp 0, _) -> ConstExp 0
      (ConstExp 1, e2) -> e2
      (e1, e2) -> MulExp e1 e2
    newState = case c of
      'w' -> regState { wReg = result }
      'x' -> regState { xReg = result }
      'y' -> regState { yReg = result }
      'z' -> regState { zReg = result }

updateRegisterForDiv :: Char -> RegExp -> RegState -> RegState
updateRegisterForDiv c exp regState = newState
  where
    currentExp = (rsLens c) regState
    result = case (currentExp, exp) of
      (ConstExp a, ConstExp b) -> ConstExp (a `quot` b)
      (e1, ConstExp 1) -> e1
      (e1, e2) -> DivExp e1 e2
    newState = case c of
      'w' -> regState { wReg = result }
      'x' -> regState { xReg = result }
      'y' -> regState { yReg = result }
      'z' -> regState { zReg = result }

updateRegisterForMod :: Char -> RegExp -> RegState -> RegState
updateRegisterForMod c exp regState = newState
  where
    currentExp = (rsLens c) regState
    result = case (currentExp, exp) of
      (ConstExp a, ConstExp b) -> ConstExp (a `mod` b)
      (e1, e2) -> ModExp e1 e2
    newState = case c of
      'w' -> regState { wReg = result }
      'x' -> regState { xReg = result }
      'y' -> regState { yReg = result }
      'z' -> regState { zReg = result }

updateRegisterForEql :: Char -> RegExp -> RegState -> RegState
updateRegisterForEql c exp regState = newState
  where
    currentExp = (rsLens c) regState
    result = case (currentExp, exp) of
      (ConstExp a, ConstExp b) -> if a == b then ConstExp 1 else ConstExp 0
      (ConstExp a, i@(InputExp _)) -> if a >= 10 || a <= 0 then ConstExp 0 else (EqlExp (ConstExp a) i)
      (e1, e2) -> EqlExp e1 e2
    newState = case c of
      'w' -> regState { wReg = result }
      'x' -> regState { xReg = result }
      'y' -> regState { yReg = result }
      'z' -> regState { zReg = result }

expToRegExp :: Exp -> RegState -> RegExp
expToRegExp (Const a) _ = ConstExp a
expToRegExp (Var 'w') rs = wReg rs
expToRegExp (Var 'x') rs = xReg rs
expToRegExp (Var 'y') rs = yReg rs
expToRegExp (Var 'z') rs = zReg rs
expToRegExp e _ = error $ "Invalid exp! " ++ show e

-- 81111111111111 is too high
-- 51111111111111 is too low
-- 74929951999389 isn't correct.
-- If I can plug in a few numbers and determine satisfiability, then I can do a logarithmic search.

{-
reduceExp :: M.Map Input Integer -> RegExp -> RegExp
reduceExp inputMap (InputExp i) = ConstExp (inputMap M.! i)
reduceExp _ (ConstExp i) = ConstExp i
reduceExp inputMap (AddExp e1 e2) = case 
-}
