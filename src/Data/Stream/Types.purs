module Data.Stream.Types where

import Prelude

import Control.Lazy as Z
import Data.Lazy (Lazy, defer, force)
import Data.Newtype (class Newtype, unwrap)

data Step a = Cons a (Stream a)

newtype Stream a = Stream (Lazy (Step a))

derive instance newtypeStream :: Newtype (Stream a) _

step :: Stream ~> Step
step = force <<< unwrap

cons :: forall a. a -> Stream a -> Stream a
cons x xs = Stream $ defer \_ -> Cons x xs

infixr 6 cons as |>
 
instance lazyStream :: Z.Lazy (Stream a) where
  defer f = Stream $ defer (step <<< f)

instance functorStream :: Functor Stream where
  map f z = 
    let (Cons x xs) = step z
    in Stream $ defer \_ -> Cons (f x) $ map f xs
