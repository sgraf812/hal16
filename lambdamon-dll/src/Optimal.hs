{-# LANGUAGE RecordWildCards #-}

module Optimal where

import Types

chooseMove GameState{..}
  | concentration < 2 && hasCoffee = Coffee
  | health > 200 && damageMultiplier < 2 = Rename
  | health <= 50 = Capture
  | otherwise = Reduce
