module Main where

import Game
import AI

import Data.List (intersperse, intercalate, transpose, maximumBy, minimumBy)
import Control.Lens.TH (makeLenses)
import Control.Lens hiding (indexed)
import Control.Monad
import Data.Ord (comparing, compare)



emptyBoard :: Int -> Board
emptyBoard n = Board $ replicate n (replicate n E)

--startBoard = emptyBoard 3
testBoard = Board [
  [X, E, O],
  [E, O, E],
  [E, E, X]
  ]

runGameMulti :: Board -> Symbol -> IO ()
runGameMulti b s = do
  putStrLn "Current board: "
  print b
  putStrLn $ "Choose your action as " ++ show s
  action <- getAction b
  let b' = move s action b
      s' = next s
  case findVictor b' of
    XWin -> print "Ay, X won!"
    OWin -> print "Ay, O won!"
    Draw -> print "It's a draw!"
    Ongoing -> runGameMulti b' s'

runGameSingle :: Board -> Symbol -> IO ()
runGameSingle b s = do
  putStrLn "Current board: "
  print b
  putStrLn $ "Choose your action as " ++ show s
  action <- getAction b
  let b' = move s action b
      s' = next s
  case findVictor b' of
    XWin -> print "Ay, X won!"
    OWin -> print "Ay, O won!"
    Draw -> print "It's a draw!"
    Ongoing ->
      print "Computer is playing..."


main :: IO ()
main = runGameMulti (emptyBoard 3) X
