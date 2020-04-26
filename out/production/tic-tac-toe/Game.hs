{-# LANGUAGE TemplateHaskell #-}

module Game
    ( Symbol (X, O, E),
      GameStatus (XWin, OWin, Draw, Ongoing),
      Board (Board), board, _board,
      Action,
      findVictor,
      validAction,
      move,
      next,
      getAction
    ) where

import Data.List (intersperse, intercalate, transpose)
import Control.Lens.TH (makeLenses)
import Control.Lens
import Control.Monad
import Control.Lens.Combinators (makeLenses)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Safe (readMay, atMay)


mapTuple :: (a -> a) -> (b -> b) -> (a, b) -> (a, b)
mapTuple f g (a, b) = (f a, g b)

data Symbol = X | O | E deriving (Eq)

data GameStatus = XWin | OWin | Draw | Ongoing deriving (Show, Eq) -- X/O/Draw/Ongoing

next :: Symbol -> Symbol
next X = O
next O = X
next E = E

instance Show Symbol where
  show X = "X"
  show O = "O"
  show E = "."

newtype Board = Board {_board :: [[Symbol]]}

instance Show Board where
  show (Board board) = intercalate "\n" lines ++ "\n"
    where lines = map (concatMap show) board

makeLenses ''Board

type Action = (Int, Int)

validAction :: Board -> Action -> Bool
validAction b (row, col) =
  case ms of
    Just E -> True
    _ -> False
  where
    ms = b ^? board . ix row . ix col

getDiag :: [[a]] -> [a] -- unsafe - only works for square matrices
getDiag xs = [xs !! i !! i | i <- [0..len] ]
  where len = length xs - 1

getDiag' :: [[a]] -> [a]
getDiag' xs = [xs !! i !! (len-i) | i <- [0..len]]
  where len = length xs - 1

isVictory :: Board -> Symbol -> Bool
isVictory b s =
  let rows = b ^. board
      cols = transpose rows
   in any (all (== s)) rows ||
      any (all (== s)) cols ||
      all (== s) (getDiag rows) ||
      all (== s) (getDiag' rows)

isDraw :: Board -> Bool
isDraw b = notElem E $ join $ b ^. board

findVictor :: Board -> GameStatus
findVictor b
  | isVictory b X = XWin
  | isVictory b O = OWin
  | isDraw b = Draw
  | otherwise = Ongoing

move :: Symbol -> Action -> Board -> Board
move s (row, col) b = b & board . ix row . ix col .~ s

getAction' :: MaybeT IO Action
getAction' = do
  inp <- lift getLine
  let nums = (map readMay . words) inp :: [Maybe Int]
  let action' = liftM2 (,) (join $ nums `atMay` 0) (join $ nums `atMay` 1) :: Maybe (Int, Int)
      action = fmap (mapTuple (subtract 1) (subtract 1)) action'  -- allow indexing from 1 in input
  MaybeT $ return action

getAction :: Board -> IO Action
getAction b = do
  action <- runMaybeT getAction'
  case action of
    Nothing -> putStrLn "Try again! Input malformatted" >> getAction b
    Just act ->
      if validAction b act
      then return act
      else putStrLn "Try again! Spot already taken or out of bounds." >> getAction b
