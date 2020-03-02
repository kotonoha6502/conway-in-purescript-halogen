module Main where

import Prelude

import Conway.AppM (runAppM)
import Conway.Env (Env)
import Conway.Page as App
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.HTML as HH

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let
    env :: Env
    env = { delta: 1000
          , pause: true }

    app :: H.Component HH.HTML App.Query App.Input App.Message Aff
    app = H.hoist (runAppM env) App.component

  halogenIO <- runUI app unit body
  pure unit