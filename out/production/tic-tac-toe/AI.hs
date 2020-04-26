module AI
  ( boardValue,
    getPossibleActions,
    getSuccessorStates
  ) where

import Game
import Control.Lens
import Data.List
import Data.Ord

argmax :: (Ord b) => (a -> b) -> [b] -> a
argmax f l
    | null l = error "Given list is empty"
    | otherwise = maximumBy (comparing f) l

--argmax :: (Ord a) =>

boardValue :: Symbol -> Board -> Double
boardValue s b =
  case (findVictor b, s) of
    (XWin, X) -> 1
    (XWin, O) -> -1
    (OWin, O) -> 1
    (OWin, X) -> -1
    (Draw, _) -> 0
    _ -> maxValue
  where
    states = getSuccessorStates s b :: [Board]
    values = map (negate . boardValue (next s)) states :: [Double]
    maxValue = maximum values

getPossibleActions :: Board  -> [Action]
getPossibleActions b = [(r, c) | r <- [0 .. len], c <- [0 .. len], validAction b (r, c)]
  where
    len = length (b ^. board) - 1


getSuccessorStates :: Symbol -> Board -> [Board]
getSuccessorStates s b = (b &) <$> map (move s) actions
  where
    actions = getPossibleActions b


--chooseAction :: Symbol -> Board -> Action
--chooseAction s b = let
--  states = getSuccessorStates s b
--  values = map (boardValue (next s)) states
--  (idx, action) = minimumBy
