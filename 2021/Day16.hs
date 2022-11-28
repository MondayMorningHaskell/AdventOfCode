{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day16 where

import Text.Megaparsec (ParsecT, some, single, choice, anySingle, count, runParserT, MonadParsec (eof, try), (<|>), many)
import Text.Megaparsec.Char (hexDigitChar)
import Data.Void (Void)
import Data.Text (Text, pack)
import Utils (Bit (..), parseLinesFromFile, parseHexadecimal, parseHexChar, bitsToDecimal8, bitsToDecimal64, parseFile)
import Control.Monad.Logger (MonadLogger, logErrorN, runStdoutLoggingT, logDebugN)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad (mzero, forM, replicateM, when, forM_)
import Data.Word (Word8, Word64)
import Control.Monad.Cont (lift)
import Control.Monad.Extra (concatMapM)

d16ES :: IO (Maybe Int)
d16ES = solveDay16Easy "inputs/day_16_small.txt"

d16EB :: IO (Maybe Int)
d16EB = solveDay16Easy "inputs/day_16_big.txt"

d16HS :: IO (Maybe Int)
d16HS = solveDay16Hard "inputs/day_16_small.txt"

d16HB :: IO (Maybe Int)
d16HB = solveDay16Hard "inputs/day_16_big.txt"

solveDay16Easy :: String -> IO (Maybe Int)
solveDay16Easy fp = runStdoutLoggingT $ do
  hexLine <- parseFile parseHexadecimal fp
  result <- runMaybeT $ do
    bitLine <- concatMapM parseHexChar hexLine
    packet <- parseBits bitLine
    return $ sumPacketVersions packet
  return (fromIntegral <$> result)

solveDay16Hard :: String -> IO (Maybe Int)
solveDay16Hard fp = runStdoutLoggingT $ do
  hexLine <- parseFile parseHexadecimal fp
  result <- runMaybeT $ do
    bitLine <- concatMapM parseHexChar hexLine
    packet <- parseBits bitLine
    calculatePacketValue packet
  return (fromIntegral <$> result)

parseBits :: (MonadLogger m) => [Bit] -> MaybeT m PacketNode
parseBits bits = do
  result <- runParserT parsePacketNode "Utils.hs" bits
  case result of
    Left e -> logErrorN ("Failed to parse: " <> (pack . show $ e)) >> mzero
    Right (packet, _) -> return packet

data PacketNode =
  Literal Word8 Word64 |
  Operator Word8 Word8 [PacketNode]
  deriving (Show)

sumPacketVersions :: PacketNode -> Word64
sumPacketVersions (Literal v _) = fromIntegral v
sumPacketVersions (Operator v _ packets) = fromIntegral v +
  sum (map sumPacketVersions packets)

calculatePacketValue :: MonadLogger m => PacketNode -> MaybeT m Word64
calculatePacketValue (Literal _ x) = return x
calculatePacketValue (Operator _ 0 packets) = sum <$> mapM calculatePacketValue packets
calculatePacketValue (Operator _ 1 packets) = product <$> mapM calculatePacketValue packets
calculatePacketValue (Operator _ 2 packets) = minimum <$> mapM calculatePacketValue packets
calculatePacketValue (Operator _ 3 packets) = maximum <$> mapM calculatePacketValue packets
calculatePacketValue (Operator _ 5 packets) = do
  if length packets /= 2
    then logErrorN "> operator '5' must have two packets!" >> mzero
    else do
      let [p1, p2] = packets
      v1 <- calculatePacketValue p1
      v2 <- calculatePacketValue p2
      return (if v1 > v2 then 1 else 0)
calculatePacketValue (Operator _ 6 packets) = do
  if length packets /= 2
    then logErrorN "< operator '6' must have two packets!" >> mzero
    else do
      let [p1, p2] = packets
      v1 <- calculatePacketValue p1
      v2 <- calculatePacketValue p2
      return (if v1 < v2 then 1 else 0)
calculatePacketValue (Operator _ 7 packets) = do
  if length packets /= 2
    then logErrorN "== operator '7' must have two packets!" >> mzero
    else do
      let [p1, p2] = packets
      v1 <- calculatePacketValue p1
      v2 <- calculatePacketValue p2
      return (if v1 == v2 then 1 else 0)
calculatePacketValue p = do
  logErrorN ("Invalid packet! " <> (pack . show $ p))
  mzero

parsePacketNode :: (MonadLogger m) => ParsecT Void [Bit] m (PacketNode, Word64)
parsePacketNode = do
  packetVersion <- parse3Bit
  packetTypeId <- parse3Bit
  if packetTypeId == 4
    then do
      (literalValue, literalBits) <- parseLiteral
      return (Literal packetVersion literalValue, literalBits + 6)
    else do
      lengthTypeId <- parseBit
      if lengthTypeId == One
        then do
          numberOfSubpackets <- bitsToDecimal64 <$> count 11 parseBit
          (subPacketsWithLengths :: [(PacketNode, Word64)]) <- replicateM (fromIntegral numberOfSubpackets) parsePacketNode
          let (subPackets :: [PacketNode], lengths :: [Word64]) = unzip subPacketsWithLengths
          return (Operator packetVersion packetTypeId subPackets, sum lengths + 7 + 11)
        else do
          totalSubpacketsLength <- bitsToDecimal64 <$> count 15 parseBit
          (subPackets, size) <- parseForPacketLength (fromIntegral totalSubpacketsLength) 0 []
          return (Operator packetVersion packetTypeId subPackets, size + 7 + 15)

parseForPacketLength :: (MonadLogger m) => Int -> Word64 -> [PacketNode] -> ParsecT Void [Bit] m ([PacketNode], Word64)
parseForPacketLength remainingBits accumBits prevPackets = if remainingBits <= 0
  then do
    if remainingBits < 0
      then lift (logErrorN "Parsed too many bits!") >> mzero
      else return (reverse prevPackets, accumBits)
  else do
    (newPacket, size) <- parsePacketNode
    parseForPacketLength (remainingBits - fromIntegral size) (accumBits + size) (newPacket : prevPackets)

-- First result is the value, second is the number of bits parsed
parseLiteral :: ParsecT Void [Bit] m (Word64, Word64)
parseLiteral = parseLiteralTail [] 0
  where
    parseLiteralTail :: [Bit] -> Word64 -> ParsecT Void [Bit] m (Word64, Word64)
    parseLiteralTail accumBits numBits = do
      leadingBit <- parseBit
      nextBits <- count 4 parseBit
      let accum' = accumBits ++ nextBits
      let numBits' = numBits + 5
      if leadingBit == Zero
        then return (bitsToDecimal64 accum', numBits')
        else parseLiteralTail accum' numBits'

parse3Bit :: ParsecT Void [Bit] m Word8
parse3Bit = bitsToDecimal8 <$> count 3 parseBit

parseBit :: ParsecT Void [Bit] m Bit
parseBit = anySingle

showBitList :: MonadLogger m => [Bit] -> m ()
showBitList bits =
  let txt = pack . show $ concatMap show bits
  in  logDebugN txt