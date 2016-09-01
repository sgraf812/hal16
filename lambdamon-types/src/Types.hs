{-# LANGUAGE DeriveDataTypeable #-}

module Types
  ( Move(..)
  , GameState(..)
  , Plugin(..)
  ) where

import Data.Typeable

data Move
  = Reduce
  | Rename
  | Capture
  | Coffee
  deriving (Eq, Ord, Show, Bounded, Enum, Read, Typeable)

data GameState = GameState
  { health :: Int
  , damageMultiplier :: Int
  , concentration :: Int
  , hasCoffee :: Bool
  } deriving (Eq, Ord, Show, Typeable)

newtype Plugin = Plugin
  { runPlugin :: GameState -> IO Move
  }
