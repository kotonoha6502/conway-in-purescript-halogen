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
import Data.Array (length, (!!), (..))
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 (colMd12, containerFluid, row)

type Input =
  { configuration :: Array (Array Boolean)
  }

type State =
  { configuration :: Array (Array Boolean)
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

    render { configuration } = 
      let rowLength = length configuration
          makeRow row =
            let colLength = length <<< fromMaybe [] <<< flip (!!) row $ configuration
            in HH.tr_ $ (0 .. (colLength - 1)) <#> \col ->
                 let isAlive = fromMaybe false $ (configuration !! row) >>= flip (!!) col
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
                    map makeRow (0 .. (rowLength - 1))
                  ]
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