module Conway.Data.Conway
  ( module Conway.Data.Conway.Types
  , fill
  ) where

import Prelude

import Conway.Data.Conway.Types (Board, Conway, conway, runConway, focus)
import Data.Array.NonEmpty (replicate, (!!))
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))

fill :: forall a. Int -> Int -> a -> Conway a
fill w h x = conway $ replicate h (replicate w x)

updateAt :: forall a. Int -> Int -> a -> Board a -> Maybe (Board a)
updateAt i j x b = case b !! i of
  Nothing -> Just b
  Just xs -> case (xs !! j) of
    Nothing -> Just b
    Just x' -> (NEA.updateAt j x xs) >>= \xs' -> NEA.updateAt i xs' b

init :: Maybe (Board Boolean)
init = pure (replicate 7 (replicate 7 false))
  >>= updateAt 0 1 true
  >>= updateAt 1 5 true
  >>= updateAt 1 0 true
  >>= updateAt 2 4 true
  >>= updateAt 6 4 true
  >>= updateAt 6 6 true
  
