{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types
  ( Move(..)
  , GameState(GameState)
  , HasHealth(..)
  , HasConcentration(..)
  , HasDamageMultiplier(..)
  , HasHasCoffee(..)
  , Plugin(..)
  ) where

import Lens.Micro.Platform

data Move
  = Reduce
  | Rename
  | Capture
  | Coffee
  deriving (Eq, Ord, Show, Bounded, Enum, Read)

data GameState = GameState
  { gameStateHealth :: Int
  , gameStateDamageMultiplier :: Int
  , gameStateConcentration :: Int
  , gameStateHasCoffee :: Bool
  }

makeFields ''GameState

newtype Plugin = Plugin
  { runPlugin :: GameState -> IO Move
  }
