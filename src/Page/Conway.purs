module Conway.Page.Conway
  ( component
  , Input
  , Query
  , Message
  ) where

import Prelude

import CSS (marginBottom, marginTop, px)
import Conway.Component.Board as GameBoard
import Conway.Component.EditPanel as EditPanel
import Conway.Data.Game (defaultLegendId)
import Conway.Data.Game as Game
import Conway.Data.Grid (modifyAt)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 (colMd10, containerFluid, row, textCenter)

type Input = Unit
type Query = Const Void
type Message = Void

data Action
  = WidthChanged Int
  | HeightChanged Int
  | StepChanged Int
  | LegendChanged String
  | RunStatusChanged
  | ResetFired
  | NextGeneration
  | AliveToggled Int Int

type State =
  { width :: Int
  , height :: Int
  , step :: Int
  , legend :: String
  , isRunning :: Boolean
  , configuration :: Game.Board
  , genCount :: Int
  }

component :: forall m. MonadEffect m => MonadAff m => H.Component HH.HTML Query Input Message m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    }
  }
  where
    initialState :: Input -> State
    initialState _ =
      { width: 16
      , height: 14
      , isRunning: false
      , step: 500
      , configuration: Game.initialState
      , genCount: 0
      , legend: defaultLegendId
      }
    
    render { height, width, step, isRunning, configuration, genCount, legend } =
        HH.div [ HP.class_ containerFluid ]
          [ HH.div [ HP.class_ row ]
            [ HH.div [ HP.classes [colMd10, ClassName "offset-md-1", textCenter]]
              [ HH.div [ HP.class_ $ ClassName "conway-content" ]
                [ HH.h1 [ HP.class_ $ ClassName "conway-title" ]
                  [ HH.text "🌵らいふげ〜む🌵"]
                , HH.div [ HP.class_ $ ClassName "conway-content-body" ]
                  [ HH.div [ HP.class_ $ ClassName "conway-config-panel-area", panelStyle ]
                    [ HH.slot (SProxy :: _ "editPanel" ) unit EditPanel.component { width, height, step, isRunning, legend } handleEditPanelMessage ]
                  , HH.div [ HP.class_ $ ClassName "conway-game-board-area", boardStyle ]
                    [ HH.slot (SProxy :: _ "board" ) unit GameBoard.component { width, height, configuration, genCount, legend } handleGameBoardMessage ]
                  ]
                ]
              ]
            ]
          ]

    handleAction = case _ of
      WidthChanged w_ -> do
        let w = min 50 <<< max 5 $ w_
        st <- H.get
        H.put $ st { width = w }
      
      HeightChanged h_ -> do
        let h = min 35 <<< max 5 $ h_
        st <- H.get
        H.put $ st { height = h }
      
      StepChanged s -> do
        st <- H.get
        H.put $ st { step = s }

      LegendChanged id -> do
        st <- H.get
        H.put $ st { legend = id }

      RunStatusChanged -> do
        st <- H.get
        H.put $ st { isRunning = not st.isRunning}

      ResetFired -> do
        H.put $ initialState unit
      
      NextGeneration -> do
        st <- H.get
        let cu = st.configuration
        let nx = Game.next st.width st.height st.configuration
        H.put $ st { configuration = nx, genCount = st.genCount + 1 }
      
      AliveToggled i j -> do
        st <- H.get
        let nx = modifyAt i j not st.configuration
        H.put $ st { configuration = nx }

    handleEditPanelMessage :: EditPanel.Message → Maybe Action
    handleEditPanelMessage = case _ of
      EditPanel.WidthChanged w -> Just <<< WidthChanged $ w
      EditPanel.HeightChanged h -> Just <<< HeightChanged $ h
      EditPanel.RunButtonClicked -> Just RunStatusChanged
      EditPanel.ResetButtonClicked -> Just ResetFired
      EditPanel.Advanced -> Just NextGeneration
      EditPanel.StepChanged s -> Just <<< StepChanged $ s 
      EditPanel.LegendChanged id -> Just <<< LegendChanged $ id
    
    handleGameBoardMessage :: GameBoard.Message -> Maybe Action
    handleGameBoardMessage = case _ of
      GameBoard.ToggleAlive i j -> Just $ AliveToggled i j

    panelStyle = CSS.style do
      marginTop $ px 8.0
      marginBottom $ px 8.0

    boardStyle = CSS.style do
      marginTop $ px 16.0
      marginBottom $ px 8.0