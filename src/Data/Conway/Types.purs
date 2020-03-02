module Conway.Data.Conway.Types
  ( Board
  , Conway
  , runConway
  , conway
  , focus
  ) where

import Prelude

import Control.Comonad (class Comonad)
import Control.Comonad.Store (Store, store)
import Control.Extend (class Extend)
import Data.Array.NonEmpty (NonEmptyArray, unsafeIndex)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

type Board a = NonEmptyArray (NonEmptyArray a)

newtype Conway a = Conway (Store (Tuple Int Int) a)

derive newtype instance functorBoard :: Functor Conway

runConway :: Conway ~> Store (Tuple Int Int)
runConway (Conway s) = s

conway :: forall a. Board a -> Conway a
conway board = Conway $ store (\(Tuple i j) -> unsafePartial $ focus i j board) (Tuple 0 0)

focus :: forall a. Partial => Int -> Int -> Board a -> a
focus i j board = unsafePartial( unsafePartial (board `unsafeIndex` i) `unsafeIndex` j)

derive newtype instance extendConway :: Extend Conway

derive newtype instance comonadConway :: Comonad Conway