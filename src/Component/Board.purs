module Conway.Component.Board
  ( Input
  , State
  , component
  , Message(..)
  ) where

import Prelude

import CSS (backgroundColor, color, display, fontSize, height, inlineBlock, px, rgb, width)
import CSS.Common (middle)
import CSS.VerticalAlign (verticalAlign)
import Conway.Data.Game (aliveLegend, defaultLegend)
import Conway.Data.Grid (Grid, index)
import Data.Array (find, (..))
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 (centerBlock, colMd12, containerFluid, row)

type Input =
  { configuration :: Grid Boolean
  , height :: Int
  , width :: Int
  , genCount :: Int
  , legend :: String
  }

type State =
  { configuration :: Grid Boolean
  , height :: Int
  , width :: Int
  , genCount :: Int
  , legend :: String
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

    initialState { configuration, height, width, genCount, legend } =
      { configuration
      , height
      , width
      , genCount
      , legend
      }

    receive :: Input -> Maybe Action
    receive input = Just $ ReceiveNextInput input

    handleAction = case _ of
      ReceiveNextInput input -> do
        st <- H.get
        H.put$  st { configuration = input.configuration
                   , height = input.height
                   , width = input.width
                   , genCount = input.genCount
                   , legend = input.legend
                   }
        
      CellClicked i j -> do
        H.raise $ ToggleAlive i j

    render { width, height, configuration, genCount, legend } = 
      let character = fromMaybe defaultLegend $ snd <$> (find (\(Tuple id _) -> id == legend) $ aliveLegend)

          makeRow row =
            HH.tr_ $ (0 .. (width - 1)) <#> \col ->
               let isAlive = index configuration row col
               in  HH.td [ tdStyle
                         , HE.onClick \_ -> Just $ CellClicked row col
                         ]
                     [ HH.text $ if isAlive then character else "." ]
    
      in
        HH.div [ HP.classes [ClassName "conway-game-board", containerFluid] ]
            [ HH.div [ HP.classes [row] ]
              [ HH.div [ HP.classes [colMd12] ]
                [ HH.div [ boardStyle ]
                  [ HH.table [ tableStyle, HP.classes [centerBlock] ]
                    [ HH.tbody [] $
                      map makeRow (0 .. (height - 1))
                    ]
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

    boardStyle = CSS.style do
      display inlineBlock

    tableStyle = CSS.style do
      fontSize $ px 20.0
      backgroundColor $ rgb 27 82 121
    
    tdStyle = CSS.style do
      height $ px 18.0
      width $ px 18.0
      color $ rgb 192 192 192
      fontSize $ px 12.0
      verticalAlign middle