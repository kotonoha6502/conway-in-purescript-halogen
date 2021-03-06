module Main where

import Prelude

import Conway.Page.Conway as Conway
import Data.Const (Const)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type AppState = Unit
type AppQuery = Const Void
type AppInput = Unit
type AppMessage = Void
type AppAction = Unit

app :: forall m. MonadEffect m => MonadAff m => H.Component HH.HTML AppQuery AppInput AppMessage m
app = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval $ H.defaultEval
  }
  where
    render _ =
      HH.div [ HP.id_ "app" ]
             [ HH.slot (SProxy :: _ "view" ) unit Conway.component unit absurd ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  halogenIO <- runUI app unit body
  pure unit