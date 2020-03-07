module Data.Zipper
  ( module Data.Zipper.Types
  , focus
  , left
  , right
  , fromStream
  , fromFoldable
  , symmetric
  , toArray
  , modifyAt
  , index
  ) where

import Prelude

import Control.Comonad (extract)
import Data.Array as A
import Data.Foldable (class Foldable)
import Data.Stream (Step(..), Stream, repeat, step, take, uncons, (!!))
import Data.Stream as S
import Data.Zipper.Types (Zipper(..), shiftl, shiftr)

focus :: forall a. Zipper a -> a
focus = extract

left :: forall a. Zipper a -> Stream a
left (Zipper ls _ _) = ls

right :: forall a. Zipper a -> Stream a
right (Zipper _ _ rs) = rs

fromStream :: forall a. a -> Stream a -> Zipper a
fromStream x xs = 
  let (Cons y ys) = step xs
  in  Zipper (repeat x) y ys

fromFoldable :: forall t a. Foldable t => a -> t a -> Zipper a
fromFoldable x xs =
  let ys = S.fromFoldable x xs
      { head: y, tail: ys' } = uncons ys
  in Zipper (repeat x) y ys'

symmetric :: forall a. a -> Zipper a
symmetric a = Zipper (repeat a) a (repeat a)

toArray :: Int -> Zipper ~> Array
toArray n = A.fromFoldable <<< take n <<< right <<< shiftl

modifyAt :: forall a. Int -> (a -> a) -> Zipper a -> Zipper a
modifyAt 0 f (Zipper ls c rs) = Zipper ls (f c) rs
modifyAt n f (Zipper ls c rs)
 | n < 0      = Zipper (S.modifyAt ((-1) * n - 1) f ls) c rs
 | otherwise  = Zipper ls c (S.modifyAt (n - 1) f rs)

index :: forall a. Zipper a -> Int -> a
index (Zipper _ c _) 0 = c
index (Zipper ls _ rs) n
 | n < 0      = ls !! ((-1) * n - 1)
 | otherwise  = rs !! (n - 1)