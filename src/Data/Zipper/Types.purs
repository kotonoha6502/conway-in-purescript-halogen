module Data.Zipper.Types where

import Prelude

import Data.Stream (Step(..), Stream, iterate, step, tail, (|>))
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)

data Zipper a = Zipper (Stream a) a (Stream a)

shiftl :: forall a. Zipper a -> Zipper a
shiftl (Zipper ls c rs) = 
  let (Cons l ls') = step ls
  in Zipper ls' l (c |> rs)

shiftr :: forall a. Zipper a -> Zipper a
shiftr (Zipper ls c rs) = 
  let (Cons r rs') = step rs
  in Zipper (c |> ls) r rs'

instance functorStream :: Functor Zipper where
  map f (Zipper ls c rs) = Zipper (f <$> ls) (f c) (f <$> rs)

instance extendZipper :: Extend Zipper where
  extend f = map f <<< dupl
    where
      dupl :: forall a. Zipper a -> Zipper (Zipper a)
      dupl z = Zipper (tail $ iterate shiftl z) z (tail $ iterate shiftr z)
                      
instance comonadZipper :: Comonad Zipper where
  extract (Zipper _ c _) = c
