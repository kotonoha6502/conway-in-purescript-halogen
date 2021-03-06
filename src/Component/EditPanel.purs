module Conway.Component.EditPanel where

import Prelude

import CSS (display, inlineBlock, marginRight, px)
import Conway.Data.Game (aliveLegend)
import Data.Const (Const)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 (btn, btnPrimary, colMd1, colMd12, colMd3, colMd4, colMd5, colMd6, formControl, formGroup, row, textLeft)
import Web.Event.Event (preventDefault)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, code, toEvent)

type Input =
  { width :: Int
  , height :: Int
  , isRunning :: Boolean
  , legend :: String
  , step :: Int
  }

type State =
  { width :: Int
  , height :: Int
  , isRunning :: Boolean
  , step :: Int
  , legend :: String
  }

data Action
  = HandleWidthInput String
  | HandleWidthKeyDown KeyboardEvent
  | HandleHeightInput String
  | HandleHeightKeyDown KeyboardEvent
  | HandleStepInput String
  | HandleStepKeyDown KeyboardEvent
  | HandleLegendSelect String
  | ForwardRunButtonClicked
  | ForwardResetButtonClicked
  | NextInputReceived Input
  | AdvanceBtnClicked

data Message
  = WidthChanged Int
  | HeightChanged Int
  | StepChanged Int
  | LegendChanged String
  | RunButtonClicked
  | ResetButtonClicked
  | Advanced

component :: forall m. MonadEffect m => MonadAff m => H.Component HH.HTML (Const Void) Input Message m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = receive
      }
  }
  where
    initialState :: Input -> State
    initialState = identity

    receive :: Input -> Maybe Action
    receive input = Just $ NextInputReceived input

    render :: State -> H.ComponentHTML Action () m
    render { height, width, isRunning, step, legend } =
      HH.div [ editPanelStyle ]
        [ HH.div [ HP.classes [row, formGroup] ]
          [ HH.div [ HP.classes [colMd5] ]
            [ HH.div [ HP.classes [row] ]
              [ HH.div [ HP.classes [colMd4] ] [ HH.text "サイズ"]
              , HH.div [ HP.classes [colMd3] ]
                [ HH.input [ HP.type_ HP.InputText , HP.classes [formControl], HP.value $ show width
                           , HP.disabled isRunning
                           , HE.onValueInput $ Just <<< HandleWidthInput
                           , HE.onKeyDown $ Just <<< HandleWidthKeyDown
                           ]
                ]
              , HH.div [ HP.classes [colMd1] ] [ HH.text "✕" ]
              , HH.div [ HP.classes [colMd3] ]
                [ HH.input [ HP.type_ HP.InputText , HP.classes [formControl], HP.value $ show height
                           , HP.disabled isRunning
                           , HE.onValueInput $ Just <<< HandleHeightInput
                           , HE.onKeyDown $ Just <<< HandleHeightKeyDown
                           ]
                ]
              ]
            ]
          , HH.div [ HP.classes [colMd3] ]
            [ HH.div [ HP.classes [row] ]
              [ HH.div [ HP.classes [colMd6] ] [ HH.text "ステップ\n/ms"]
              , HH.div [ HP.classes [colMd6] ]
                [ HH.input [ HP.type_ HP.InputText , HP.classes [formControl], HP.value $ show step
                           , HP.disabled isRunning
                           , HE.onValueInput $ Just <<< HandleStepInput
                           , HE.onKeyDown $ Just <<< HandleStepKeyDown
                           ]
                ]
              ]
            ]
          , HH.div [ HP.classes [colMd3] ]
            [ HH.div [ HP.classes [row] ]
              [ HH.div [ HP.classes [colMd6] ] [ HH.text "凡例" ]
              , HH.div [ HP.classes [colMd6] ]
                [ HH.select [ HP.classes [formControl] 
                            , HE.onValueChange $ Just <<< HandleLegendSelect 
                            ] $
                  aliveLegend <#> \(Tuple id leg) ->
                    HH.option [ HP.value id, HP.selected (id == legend) ] [ HH.text leg ]
                ]
              ]
            ]
          ]
        , HH.div [ HP.classes [row] ]
          [ HH.div [ HP.classes [colMd12, textLeft] ]
            [ HH.button [ HP.classes [btn, btnPrimary], btnStyle 
                        , HE.onClick \_ -> Just ForwardRunButtonClicked 
                        ]
              [ HH.text (if isRunning then "すとっぷ" else "すた〜と") ]
            , HH.button [ HP.classes [btn, ClassName "btn-outline-primary"], btnStyle
                        , HE.onClick \_ -> Just ForwardResetButtonClicked
                        ]
              [ HH.text "りせっと" ]
            , HH.button [ HP.classes [btn, ClassName "btn-outline-primary"], btnStyle
                        , HE.onClick \_ -> Just AdvanceBtnClicked
                        ]
              [ HH.text "+1世代" ]
            ]
          ]
        ]
    
    handleAction :: Action -> H.HalogenM State Action () Message m Unit
    handleAction = case _ of
      NextInputReceived input -> do
        H.put input

      ForwardResetButtonClicked -> do
        H.raise $ ResetButtonClicked

      HandleWidthInput str -> do
        currentSt <- H.get
        let new = fromMaybe currentSt.width <<< fromString $ str
        when (new /= currentSt.width) do
          H.raise $ WidthChanged new

      HandleWidthKeyDown e -> case code e of
        "ArrowUp" -> do
          H.liftEffect $ preventDefault (toEvent e)
          st <- H.get
          H.raise $ WidthChanged (st.width + 1)

        "ArrowDown" -> do
          H.liftEffect $ preventDefault (toEvent e)
          st <- H.get
          when (st.width > 1) do
            H.raise $ WidthChanged (st.width - 1)

        _ -> pure unit

      HandleHeightInput str -> do
        currentSt <- H.get
        let new = fromMaybe currentSt.height <<< fromString $ str
        when (new /= currentSt.height) $
          H.raise $ HeightChanged new

      HandleHeightKeyDown e -> case code e of
        "ArrowUp" -> do
          H.liftEffect $ preventDefault (toEvent e)
          st <- H.get
          H.raise $ HeightChanged (st.height + 1)

        "ArrowDown" -> do
          H.liftEffect $ preventDefault (toEvent e)
          st <- H.get
          when (st.height > 1) do
            H.raise $ HeightChanged (st.height - 1)

        _ -> pure unit

      HandleStepInput str -> do
        currentSt <- H.get
        let new =fromMaybe currentSt.step <<< fromString $ str
        when (new /= currentSt.step) $
          H.raise $ StepChanged new

      HandleStepKeyDown e -> case code e of
        "ArrowUp" -> do
          H.liftEffect $ preventDefault (toEvent e)
          st <- H.get
          H.raise $ StepChanged (st.step + 100)

        "ArrowDown" -> do
          H.liftEffect $ preventDefault (toEvent e)
          st <- H.get
          when (st.step > 100) do
            H.raise $ StepChanged (st.step - 100)
            H.put $ st { step = st.step - 100 }

        _ -> pure unit

      ForwardRunButtonClicked -> do
        st <- H.get
        let shouldAdvanceAuto = not st.isRunning
        H.put $ st { isRunning = shouldAdvanceAuto }
        H.raise $ RunButtonClicked

        let autoAdvance = do
              { step } <- H.get
              H.liftAff $ Aff.delay $ Aff.Milliseconds (toNumber step)
              { isRunning } <- H.get
              if isRunning
              then do
                H.raise $ Advanced
                autoAdvance
              else pure unit

        when shouldAdvanceAuto autoAdvance

      HandleLegendSelect id -> do
        H.raise $ LegendChanged id    
      
      AdvanceBtnClicked -> do
        H.raise Advanced

    editPanelStyle = CSS.style do
      display inlineBlock
    
    btnStyle = CSS.style do
      marginRight $ px 32.0
