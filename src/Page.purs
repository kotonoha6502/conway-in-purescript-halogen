module Conway.Page
  ( component
  , Input
  , Query
  , Message
  ) where

import Prelude

import Data.Const (Const)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Input = Unit
type Query = Const Void
type Message = Void
type Action = Unit

component :: forall m. Monad m => H.Component HH.HTML Query Input Message m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
  }
  where
    initialState = \_ -> unit
    render _ =
      HH.div [ HP.class_ $ ClassName "conway-content" ]
             [ HH.h1 [ HP.class_ $ ClassName "conway-title" ]
                     [ HH.text "🌵らいふげ〜む🌵"]
             , HH.div [ HP.class_ $ ClassName "conway-content-body" ]
                      [ HH.text "ここにゲームボード表示"
                      ]
             ]