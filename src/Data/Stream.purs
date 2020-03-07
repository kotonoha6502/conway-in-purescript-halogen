module Data.Stream 
  ( module Data.Stream.Types
  , uncons
  , head
  , tail
  , iterate
  , repeat
  , index
  , (!!)
  , fromFoldable
  , take
  , modifyAt
  , alterAt
  ) where

import Prelude

import Control.Lazy (defer, fix)
import Data.Foldable (class Foldable, foldr)
import Data.List.Lazy (List, nil, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Stream.Types (Step(..), Stream(..), cons, step, (|>))

uncons :: forall a. Stream a -> { head :: a, tail :: Stream a }
uncons zs =
  let (Cons z zs') = step zs
  in { head: z, tail: zs' }

head :: forall a. Stream a -> a
head = _.head <<< uncons

tail :: forall a. Stream a -> Stream a
tail = _.tail <<< uncons

iterate :: forall a. (a -> a) -> a -> Stream a
iterate f x = x |> defer \_ -> iterate f (f x)

repeat :: forall a. a -> Stream a
repeat x = fix $ \xs -> x |> xs

index :: forall a. Stream a -> Int -> a
index xs = go (step xs)
  where
  go (Cons a _) 0 = a
  go (Cons _ as) i = go (step as) (i - 1)

infixl 8 index as !!

fromFoldable :: forall a t. Foldable t => a -> t a -> Stream a
fromFoldable x xs = foldr cons (repeat x) xs

take :: forall a. Int -> Stream a -> List a
take n = go n <<< step
  where
    go 0 _ = nil
    go n' (Cons x xs) = x : (take (n' - 1) xs)

modifyAt :: forall a. Int -> (a -> a) -> Stream a -> Stream a
modifyAt n f = alterAt n (Just <<< f)

alterAt :: forall a. Int -> (a -> Maybe a) -> Stream a -> Stream a
alterAt n f xs = Stream (go n <$> unwrap xs)
  where
  go 0 (Cons y ys) = case f y of
    Nothing -> step ys
    Just y' -> Cons y' ys
  go n' (Cons y ys) = Cons y (alterAt (n' - 1) f ys)