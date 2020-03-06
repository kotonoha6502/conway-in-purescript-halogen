module Conway.Data.Grid
  ( Grid(..)
  , up
  , down
  , left
  , right
  , focus
  , fromArray2
  , toArray2
  ) where

import Prelude

import Control.Comonad (class Comonad, class Extend, extract)
import Data.Newtype (class Newtype, unwrap)
import Data.Stream (fromFoldable, iterate, repeat, tail, uncons)
import Data.Zipper (Zipper(..), fromStream, shiftl, shiftr, symmetric, toArray)

newtype Grid a = Grid (Zipper (Zipper a))

derive instance newtypeGrid :: Newtype (Grid a) _

instance functorGrid :: Functor Grid where
  map f = Grid <<< map (map f) <<< unwrap

instance extendGrid :: Extend Grid where
  extend f = map f <<< dupl
    where
      dupl :: forall a. Grid a -> Grid (Grid a)
      dupl (Grid z) = map Grid $ Grid $ layer (layer z)

      layer :: forall a. Zipper (Zipper a) -> Zipper (Zipper (Zipper a))
      layer zzs = 
        let lefts = tail $ iterate (map shiftl) zzs
            rights = tail $ iterate (map shiftr) zzs
        in  Zipper lefts zzs rights

instance comonadGrid :: Comonad Grid where
  extract = focus

up :: forall a. Grid a -> Grid a
up = Grid <<< shiftl <<< unwrap

down :: forall a. Grid a -> Grid a
down = Grid <<< shiftr <<< unwrap

left :: forall a. Grid a -> Grid a
left = Grid <<< (map shiftl) <<< unwrap

right :: forall a. Grid a -> Grid a
right = Grid <<< (map shiftr) <<< unwrap

focus :: forall a. Grid a -> a
focus = extract <<< extract <<< unwrap

fromArray2 :: forall a. a -> Array (Array a) -> Grid a
fromArray2 a xss = 
  let universe = fromFoldable (symmetric a) $ fromStream a <<< fromFoldable a <$> xss
      { head: u, tail: us } = uncons universe
  in Grid $ Zipper (repeat $ symmetric a) u us

toArray2 :: forall a. Int -> Int -> Grid a -> Array (Array a)
toArray2 i j (Grid g) = toArray j (toArray i <$> g)