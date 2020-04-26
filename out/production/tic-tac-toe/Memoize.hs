module Memoize where

import Game (Board)


class Memoizable a where
  memoize :: (a -> b) -> (a -> b)

--instance Memoizable Board where
--  memoize b =