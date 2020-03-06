module Conway.Data.Game
  ( initialState
  , Board
  , rule
  , next
  ) where

import Prelude

import Control.Extend (extend)
import Conway.Data.Grid (Grid, down, focus, fromArray2, left, right, toArray2, up)
import Data.Array (filter, length)

initialState :: Grid Boolean
initialState = fromArray2 false $
    [ [ false, false, true,  true,  false, false, false, false, false, false, false, false, false, false, false, false ]
    , [ true,  false, false, true,  false, false, false, false, false, false, false, false, false, false, false, false ]
    , [ false, true,  true,  false, false, false, false, false, false, false, false, false, false, false, false, false ]
    , [ false, false, false, true,  false, false, false, false, false, false, false, false, false, false, false, false ]
    , [ false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false ]
    , [ false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false ]
    , [ false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false ]
    , [ false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false ]
    , [ false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false ]
    , [ false, false, false, false, false, false, true,  true,  true,  false, false, false, false, false, false, false ]
    , [ false, false, false, false, false, false, true,  false, true,  false, false, false, false, false, false, false ]
    , [ false, false, false, false, false, true,  true,  false, false, false, true,  false, false, false, false, false ]
    , [ false, false, false, false, false, false, false, false, false, true,  false, false, false, false, false, false ]
    , [ false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false ]
    ]

type Board = Grid Boolean

rule :: Board -> Boolean
rule b =
  let -- state of currently focused cell
      c  = focus b 

      -- state of eight neighbours   
      u  = focus <<< up $ b
      r  = focus <<< right $ b
      d  = focus <<< down $ b
      l  = focus <<< left $ b
      ur = focus <<< right <<< up $ b
      ul = focus <<< left <<< up $ b
      dr = focus <<< right <<< down $ b
      dl = focus <<< left <<< down $ b
      
      -- number of live neighbours
      n  = length <<< filter (_ == true) $ [u, r, d, l, ur, ul, dr, dl]
      
      -- will focused cell survive or become a live cell?
  in (c && (n == 2 || n == 3)) || ((not c) && n == 3)

next :: Int -> Int -> Board -> Board
next i j  = fromArray2 false <<< toArray2 i j <<< extend rule