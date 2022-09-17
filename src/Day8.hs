{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Data.List (sortOn, delete, intersect)
import Data.Maybe (catMaybes)
import Control.Monad (when, forM_, mzero)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Logger (MonadLogger, logErrorN, logDebugN, runStdoutLoggingT)
import Control.Monad.Trans.Class (lift)
import Text.Megaparsec (ParsecT, sepEndBy1, some)
import Text.Megaparsec.Char (hspace, eol, string, letterChar)
import Data.Void (Void)
import Data.Text (Text, pack, intercalate)
import Utils (parseLinesFromFile, countWhere)

d8ES :: IO (Maybe Int)
d8ES = solveDay8Easy "inputs/day_8_small.txt"

d8EB :: IO (Maybe Int)
d8EB = solveDay8Easy "inputs/day_8_big.txt"

d8HS :: IO (Maybe Int)
d8HS = solveDay8Hard "inputs/day_8_small.txt"

d8HB :: IO (Maybe Int)
d8HB = solveDay8Hard "inputs/day_8_big.txt"

solveDay8Easy :: String -> IO (Maybe Int)
solveDay8Easy fp = runStdoutLoggingT $ do
  codes <- catMaybes <$> parseLinesFromFile parseInputLine fp
  let result = sum $ uniqueOutputs <$> (snd <$> codes)
  return $ Just result

solveDay8Hard :: String -> IO (Maybe Int)
solveDay8Hard fp = runStdoutLoggingT $ do
  inputCodes <- catMaybes <$> parseLinesFromFile parseInputLine fp
  results <- runStdoutLoggingT $ runMaybeT (mapM decodeAllOutputs inputCodes)
  return $ fmap sum results

data InputCode = InputCode
  { screen0 :: String
  , screen1 :: String
  , screen2 :: String
  , screen3 :: String
  , screen4 :: String
  , screen5 :: String
  , screen6 :: String
  , screen7 :: String
  , screen8 :: String
  , screen9 :: String
  } deriving (Show)

data OutputCode = OutputCode
  { output1 :: String
  , output2 :: String
  , output3 :: String
  , output4 :: String
  } deriving (Show)

uniqueOutputs :: OutputCode -> Int
uniqueOutputs (OutputCode o1 o2 o3 o4) = countWhere isUniqueDigitCode [o1, o2, o3, o4]

isUniqueDigitCode :: String -> Bool
isUniqueDigitCode input = length input `elem` [2, 3, 4, 7]

decodeAllOutputs :: (MonadLogger m) => (InputCode, OutputCode) -> MaybeT m Int
decodeAllOutputs (ic, OutputCode o1 o2 o3 o4) = do
  d01 <- decodeString ic o1
  d02 <- decodeString ic o2
  d03 <- decodeString ic o3
  d04 <- decodeString ic o4
  return $ d01 * 1000 + d02 * 100 + d03 * 10 + d04

decodeString :: (MonadLogger m) => InputCode -> String -> MaybeT m Int
decodeString inputCodes output
  | length output == 2 = return 1
  | length output == 3 = return 7
  | length output == 4 = return 4
  | length output == 7 = return 8
  | length output == 5 = decode5 inputCodes output
  | length output == 6 = decode6 inputCodes output
  | otherwise = mzero

decode5 :: (MonadLogger m) => InputCode -> String -> MaybeT m Int
decode5 ic output = do
  ([_, _, _, c3, c4, c5, _, _, _, _], c01, c02, fourMinusOne) <- sortInputCodes ic
  -- If both from c0 are present, it's a 3
  if c01 `elem` output && c02 `elem` output
    then return 3
    else do
      let onlyShared = (intersect fourMinusOne c3 `intersect` c4) `intersect` c5
      if length onlyShared /= 1
        then logErrorN ("Invalid 5 case; expected length 1 remainder: " <> pack onlyShared) >> mzero
        -- If final character is present in output, it's a 5, else 2
        else if head (delete (head onlyShared) fourMinusOne) `elem` output then return 5 else return 2

decode6 :: (MonadLogger m) => InputCode -> String -> MaybeT m Int
decode6 ic output = do
  (_, c01, c02, fourMinusOne) <- sortInputCodes ic
  -- If not both from c0 are present, it's a 6
  if not (c01 `elem` output && c02 `elem` output)
    then return 6
    else do
      -- If both of these characters are present in output, 9 else 0
      if all (`elem` output) fourMinusOne then return 9 else return 0

-- Return 1. Sorted code strings 2, 3. Chars for "1" and String for "fourMinusOne"
sortInputCodes :: (MonadLogger m) => InputCode -> MaybeT m ([String], Char, Char, String)
sortInputCodes ic@(InputCode c0 c1 c2 c3 c4 c5 c6 c7 c8 c9) = do
  if not validLengths
    then logErrorN ("Invalid inputs: " <> (pack . show $ ic)) >> mzero
    else do
      let [sc01, sc02] = sc0
      let fourMinusOne = delete sc02 (delete sc01 sc2)
      return (sorted, sc01, sc02, fourMinusOne)
  where
    sorted@[sc0, sc1,sc2,sc3,sc4,sc5,sc6,sc7,sc8,sc9] = sortOn length [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9]
    validLengths =
      length sc0 == 2 && length sc1 == 3 && length sc2 == 4 &&
      length sc3 == 5 && length sc4 == 5 && length sc5 == 5 &&
      length sc6 == 6 && length sc7 == 6 && length sc8 == 6 &&
      length sc9 == 7

parseInputLine :: (MonadLogger m) => ParsecT Void Text m (Maybe (InputCode, OutputCode))
parseInputLine = do
  screenCodes <- sepEndBy1 (some letterChar) hspace
  string "| "
  outputCodes <- sepEndBy1 (some letterChar) hspace
  if length screenCodes /= 10 
    then lift (logErrorN $ "Didn't find 10 screen codes: " <> intercalate ", " (pack <$> screenCodes)) >> return Nothing
    else if length outputCodes /= 4
      then lift (logErrorN $ "Didn't find 4 output codes: " <> intercalate ", " (pack <$> outputCodes)) >> return Nothing
      else
        let [s0, s1, s2, s3, s4, s5, s6, s7, s8, s9] = screenCodes
            [o1, o2, o3, o4] = outputCodes
        in  return $ Just (InputCode s0 s1 s2 s3 s4 s5 s6 s7 s8 s9, OutputCode o1 o2 o3 o4)
