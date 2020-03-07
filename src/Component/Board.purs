module Conway.Component.Board
  ( Input
  , State
  , component
  , Message(..)
  ) where

import Prelude

import CSS (backgroundColor, color, fontSize, height, px, rgb, width)
import CSS.Common (middle)
import CSS.VerticalAlign (verticalAlign)
import Conway.Data.Grid (Grid, index)
import Data.Array ((..))
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 (colMd12, containerFluid, row)

type Input =
  { configuration :: Grid Boolean
  , height :: Int
  , width :: Int
  , genCount :: Int
  }

type State =
  { configuration :: Grid Boolean
  , height :: Int
  , width :: Int
  , genCount :: Int
  }

data Action
  = ReceiveNextInput Input
  | CellClicked Int Int

data Message
  = ToggleAlive Int Int

component :: forall m. MonadEffect m => H.Component HH.HTML (Const Void) Input Message m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                   , receive = receive
                                   }
  }
  where

    initialState = identity

    receive :: Input -> Maybe Action
    receive input = Just $ ReceiveNextInput input

    handleAction = case _ of
      ReceiveNextInput input -> do
        H.put input
        
      CellClicked i j -> do
        H.raise $ ToggleAlive i j

    render { width, height, configuration, genCount } = 
      let makeRow row =
            HH.tr_ $ (0 .. (width - 1)) <#> \col ->
               let isAlive = index configuration row col
               in  HH.td [ tdStyle
                         , HE.onClick \_ -> Just $ CellClicked row col
                         ]
                     [ HH.text $ if isAlive then "λ" else "." ]
    
      in
        HH.div [ HP.classes [ClassName "conway-game-board", containerFluid] ]
            [ HH.div [ HP.classes [row] ]
              [ HH.div [ HP.classes [colMd12] ]
                [ HH.table [ tableStyle ]
                  [ HH.tbody [] $
                    map makeRow (0 .. (height - 1))
                  ]
                ]
              ]
            , HH.div [ HP.classes [row] ]
              [ HH.div [HP.classes [colMd12] ]
                [ HH.span_ [ HH.text "現在：" ]
                , HH.span_ [ HH.text $ "第 " <> show genCount <> " 世代" ]
                ]
              ]
            ]

    tableStyle = CSS.style do
      fontSize $ px 20.0
      backgroundColor $ rgb 27 82 121
    
    tdStyle = CSS.style do
      height $ px 18.0
      width $ px 18.0
      color $ rgb 192 192 192
      fontSize $ px 11.0
      verticalAlign middle