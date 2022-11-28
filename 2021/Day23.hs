{-# LANGUAGE OverloadedStrings #-}

module Day23 where

import Control.Applicative ( Alternative((<|>)) )
import Control.Monad (forM_)
import qualified Data.Array as A
import Data.Maybe (isNothing, mapMaybe, fromJust, isJust, catMaybes)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Algorithm.Search ( dijkstraM )
import Control.Monad.Logger ( MonadLogger, runStdoutLoggingT, logErrorN, logDebugN )
import Data.Ix ( Ix )
import Data.Text (pack)

d23ES :: IO (Maybe Int)
d23ES = solveDay23Easy initialState1

d23EB :: IO (Maybe Int)
d23EB = solveDay23Easy initialState2

d23HS :: IO (Maybe Int)
d23HS = solveDay23Hard initialState3

d23HB :: IO (Maybe Int)
d23HB = solveDay23Hard initialState4

solveDay23Easy :: GraphState -> IO (Maybe Int)
solveDay23Easy gs = runStdoutLoggingT $ do
  result <- dijkstraM (getNeighbors 2) getCost isComplete gs
  case result of
    Nothing -> return Nothing
    Just (d, path) -> return $ Just d

solveDay23Hard :: GraphState -> IO (Maybe Int)
solveDay23Hard gs = runStdoutLoggingT $ do
  result <- dijkstraM (getNeighbors 4) getCost isComplete gs
  case result of
    Nothing -> return Nothing
    Just (d, path) -> return $ Just d

data Token = A | B | C | D
  deriving (Show, Eq, Ord, Enum, Ix)

data Room = RA | RB | RC | RD
  deriving (Show, Eq, Ord, Enum, Ix)

data HallSpace = H1 | H2 | H4 | H6 | H8 | H10 | H11
  deriving (Show, Eq, Ord, Enum, Ix)

data GraphState = GraphState
  { lastMove :: Move
  , roomsFull :: Int
  , roomA :: [Token]
  , roomB :: [Token]
  , roomC :: [Token]
  , roomD :: [Token]
  , hall1 :: Maybe Token
  , hall2 :: Maybe Token
  , hall4 :: Maybe Token
  , hall6 :: Maybe Token
  , hall8 :: Maybe Token
  , hall10 :: Maybe Token
  , hall11 :: Maybe Token
  }
  deriving (Show, Eq, Ord)

type RoomLens = GraphState -> [Token]
type HallLens = GraphState -> Maybe Token

data Move =
  NoMove |
  Move Token HallSpace (Room, Int) Bool
  deriving (Show, Eq, Ord)

initialState1 :: GraphState
initialState1 = GraphState
  NoMove 0 [B, A] [C, D] [B, C] [D, A]
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing

initialState2 :: GraphState
initialState2 = GraphState
  NoMove 0 [C, B] [A, A] [D, B] [D, C]
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing

initialState3 :: GraphState
initialState3 = GraphState
  NoMove 0 [B, D, D, A] [C, C, B, D] [B, B, A, C] [D, A, C, A]
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing

initialState4 :: GraphState
initialState4 = GraphState
  NoMove 0 [C, D, D, B] [A, C, B, A] [D, B, A, B] [D, A, C, C]
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing

isComplete :: (MonadLogger m) => GraphState -> m Bool
isComplete gs = return (roomsFull gs == 4)

getCost :: (MonadLogger m) => GraphState -> GraphState -> m Int
getCost _ gs = if lastMove gs == NoMove
  then return 0
  else do
    let (Move token hs (rm, slot) _) = lastMove gs
        multiplier = tokenPower A.! token
        distance = hallRoomDistance A.! (hs, rm) + slot
    return $ multiplier * distance

getNeighbors :: (MonadLogger m) => Int -> GraphState -> m [GraphState]
getNeighbors rs gs = do
  aMoves <- roomMoves rs A RA roomA aSplits gs
  bMoves <- roomMoves rs B RB roomB bSplits gs
  cMoves <- roomMoves rs C RC roomC cSplits gs
  dMoves <- roomMoves rs D RD roomD dSplits gs
  return $ aMoves <> bMoves <> cMoves <> dMoves

roomMoves ::
  (MonadLogger m) =>
  Int ->
  Token ->
  Room ->
  RoomLens ->
  ([(HallLens, HallSpace)], [(HallLens, HallSpace)]) ->
  GraphState ->
  m [GraphState]
roomMoves rs tok rm roomLens splits gs
  | roomLens gs == replicate rs tok = return []
  | all (== tok) (roomLens gs) = do
    let maybeLeft = findX tok gs (fst splits)
        maybeRight = findX tok gs (snd splits)
        halls = catMaybes [maybeLeft, maybeRight]
        slot = rs - length (roomLens gs)
        moves = map (\h -> Move tok h (rm, slot) False) halls
    return $ map (applyHallMove rs roomLens gs) moves
  | otherwise = do
    let (topRoom : restRoom) = roomLens gs
        slot = rs - length restRoom
        halls = findEmptyHalls gs (fst splits) [] <> findEmptyHalls gs (snd splits) []
        moves = map (\h -> Move topRoom h (rm, slot) True) halls
    return $ map (applyRoomMove gs) moves

findEmptyHalls :: GraphState -> [(HallLens, HallSpace)] -> [HallSpace] -> [HallSpace]
findEmptyHalls _ [] accum = accum
findEmptyHalls gs ((lens, space) : rest) accum = if isJust (lens gs) then accum
  else findEmptyHalls gs rest (space : accum)

findX :: Token -> GraphState -> [(HallLens, HallSpace)] -> Maybe HallSpace
findX _ _ [] = Nothing
findX tok gs ((lens, space) : rest)
  | lens gs == Just tok = Just space
  | isJust (lens gs) = Nothing
  | otherwise = findX tok gs rest

applyRoomMove :: GraphState -> Move -> GraphState
applyRoomMove gs NoMove = gs
applyRoomMove gs m@(Move token h (rm, slot) _) =
  let gs2 = case h of
        H1 -> gs {hall1 = Just token, lastMove = m}
        H2 -> gs {hall2 = Just token, lastMove = m}
        H4 -> gs {hall4 = Just token, lastMove = m}
        H6 -> gs {hall6 = Just token, lastMove = m}
        H8 -> gs {hall8 = Just token, lastMove = m}
        H10 -> gs {hall10 = Just token, lastMove = m}
        H11 -> gs {hall11 = Just token, lastMove = m}
  in  case rm of
    RA -> gs2 { roomA = tail (roomA gs)}
    RB -> gs2 { roomB = tail (roomB gs)}
    RC -> gs2 { roomC = tail (roomC gs)}
    RD -> gs2 { roomD = tail (roomD gs)}

applyHallMove :: Int -> RoomLens -> GraphState -> Move -> GraphState
applyHallMove rs roomLens gs NoMove = gs
applyHallMove rs roomLens gs m@(Move token h (rm, slot) _) =
  let gs2 = case h of
        H1 -> gs {hall1 = Nothing, lastMove = m, roomsFull = finishedCount}
        H2 -> gs {hall2 = Nothing, lastMove = m, roomsFull = finishedCount}
        H4 -> gs {hall4 = Nothing, lastMove = m, roomsFull = finishedCount}
        H6 -> gs {hall6 = Nothing, lastMove = m, roomsFull = finishedCount}
        H8 -> gs {hall8 = Nothing, lastMove = m, roomsFull = finishedCount}
        H10 -> gs {hall10 = Nothing, lastMove = m, roomsFull = finishedCount}
        H11 -> gs {hall11 = Nothing, lastMove = m, roomsFull = finishedCount}
  in  case token of
    A -> gs2 {roomA = A : roomA gs }
    B -> gs2 {roomB = B : roomB gs }
    C -> gs2 {roomC = C : roomC gs }
    D -> gs2 {roomD = D : roomD gs }
  where
    finished = length (roomLens gs) == rs - 1
    finishedCount = roomsFull gs + if finished then 1 else 0

-- Constants
aSplits :: ([(HallLens, HallSpace)], [(HallLens, HallSpace)])
aSplits =
  ( [(hall2, H2), (hall1, H1)]
  , [(hall4, H4), (hall6, H6), (hall8, H8), (hall10, H10), (hall11, H11)]
  )

bSplits :: ([(HallLens, HallSpace)], [(HallLens, HallSpace)])
bSplits =
  ( [(hall4, H4), (hall2, H2), (hall1, H1)]
  , [(hall6, H6), (hall8, H8), (hall10, H10), (hall11, H11)]
  )

cSplits :: ([(HallLens, HallSpace)], [(HallLens, HallSpace)])
cSplits =
  ( [(hall6, H6), (hall4, H4), (hall2, H2), (hall1, H1)]
  , [(hall8, H8), (hall10, H10), (hall11, H11)]
  )

dSplits :: ([(HallLens, HallSpace)], [(HallLens, HallSpace)])
dSplits =
  ( [(hall8, H8), (hall6, H6), (hall4, H4), (hall2, H2), (hall1, H1)]
  , [(hall10, H10), (hall11, H11)]
  )

hallRoomDistance :: A.Array (HallSpace, Room) Int
hallRoomDistance = A.array ((H1, RA), (H11, RD))
  [ ((H1, RA), 2), ((H1, RB), 4), ((H1, RC), 6), ((H1, RD), 8)
  , ((H2, RA), 1), ((H2, RB), 3), ((H2, RC), 5), ((H2, RD), 7)
  , ((H4, RA), 1), ((H4, RB), 1), ((H4, RC), 3), ((H4, RD), 5)
  , ((H6, RA), 3), ((H6, RB), 1), ((H6, RC), 1), ((H6, RD), 3)
  , ((H8, RA), 5), ((H8, RB), 3), ((H8, RC), 1), ((H8, RD), 1)
  , ((H10, RA), 7), ((H10, RB), 5), ((H10, RC), 3), ((H10, RD), 1)
  , ((H11, RA), 8), ((H11, RB), 6), ((H11, RC), 4), ((H11, RD), 2)
  ]

tokenPower :: A.Array Token Int
tokenPower = A.array (A, D) [(A, 1), (B, 10), (C, 100), (D, 1000)]
