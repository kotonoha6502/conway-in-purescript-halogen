module Conway.Capability.Game
  ( class MonadGame
  , run
  , stop
  ) where

import Prelude

import Effect.Aff.Class (class MonadAff)

class MonadAff m <= MonadGame m where
  run :: Unit -> m Unit
  stop :: Unit -> m Unit
