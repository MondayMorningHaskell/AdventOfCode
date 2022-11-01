{-# LANGUAGE OverloadedStrings #-}

module Day20 where

import Text.Megaparsec (ParsecT, choice, some, sepEndBy1)
import Text.Megaparsec.Char (char, eol)
import Data.Void (Void)
import Data.Text (Text, snoc)
import Utils (Bit(..), Coord2, hashMapFromNestedLists, countWhere, getNeighbors8Flex, bitsToDecimal64, parseFile, getNeighbors8Unbounded)
import Data.HashMap.Strict (HashMap)
import Data.Word (Word64)
import qualified Data.HashMap.Strict as HM
import Control.Monad.Logger (MonadLogger, logErrorN, runStdoutLoggingT)
import Control.Monad.Cont (foldM)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)
import Data.Ix (range)

d20ES :: IO (Maybe Int)
d20ES = solveDay20Easy "inputs/day_20_small.txt"

d20EB :: IO (Maybe Int)
d20EB = solveDay20Easy "inputs/day_20_big.txt"

d20HS :: IO (Maybe Int)
d20HS = solveDay20Hard "inputs/day_20_small.txt"

d20HB :: IO (Maybe Int)
d20HB = solveDay20Hard "inputs/day_20_big.txt"

solveDay20Easy :: String -> IO (Maybe Int)
solveDay20Easy fp = runStdoutLoggingT $ do
  (decoderMap, initialImage) <- parseFile parseInput fp
  pixelsLit <- runExpand decoderMap initialImage 2
  return $ Just pixelsLit

solveDay20Hard :: String -> IO (Maybe Int)
solveDay20Hard fp = runStdoutLoggingT $ do
  (decoderMap, initialImage) <- parseFile parseInput fp
  pixelsLit <- runExpand decoderMap initialImage 50
  return $ Just pixelsLit

type DecoderMap = HashMap Word64 Bit
type ImageMap = HashMap Coord2 Bit

buildDecoder :: [Bit] -> DecoderMap
buildDecoder input = HM.fromList (zip [0..] input)

parseInput :: (MonadLogger m) => ParsecT Void Text m (DecoderMap, ImageMap)
parseInput = do
  decoderMap <- buildDecoder <$> some parsePixel
  eol >> eol
  image <- hashMapFromNestedLists <$> parse2DImage
  return (decoderMap, image)

parsePixel :: (MonadLogger m) => ParsecT Void Text m Bit
parsePixel = choice [char '.' >> return Zero, char '#' >> return One]

parse2DImage :: (MonadLogger m) => ParsecT Void Text m [[Bit]]
parse2DImage = sepEndBy1 (some parsePixel) eol

runExpand :: (MonadLogger m) => DecoderMap -> ImageMap -> Int -> m Int
runExpand _ image 0 = return $ countWhere (== One) (HM.elems image)
runExpand decoderMap initialImage stepCount = do
  finalImage <- expandImage decoderMap initialImage outsideBit
  runExpand decoderMap finalImage (stepCount - 1)
  where
    outsideBit = if decoderMap HM.! 0 == Zero || even stepCount then Zero else One

expandImage :: (MonadLogger m) => DecoderMap -> ImageMap -> Bit -> m ImageMap
expandImage decoderMap image outsideBit = foldM (processPixel decoderMap image outsideBit) HM.empty allCoords
  where
    (minRow, minCol) = minimum (HM.keys image)
    (maxRow, maxCol) = maximum (HM.keys image)
    newBounds = ((minRow - 1, minCol - 1), (maxRow + 1, maxCol + 1))
    allCoords = range newBounds

processPixel :: (MonadLogger m) => DecoderMap -> ImageMap -> Bit -> ImageMap -> Coord2 -> m ImageMap
processPixel decoderMap initialImage outsideBit newImage pixel = do
  let allNeighbors = getNeighbors8Unbounded pixel
      neighborBits = getBit <$> allNeighbors
  if length allNeighbors /= 8
    then error "Must have 8 neighbors!"
    else do
      let (first4, second4) = splitAt 4 neighborBits
          finalBits = first4 ++ (getBit pixel : second4)
          indexToDecode = bitsToDecimal64 finalBits
          bit = decoderMap HM.! indexToDecode
      return $ HM.insert pixel bit newImage
  where
    getBit :: Coord2 -> Bit
    getBit coord = fromMaybe outsideBit (initialImage HM.!? coord)
