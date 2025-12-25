module Demo.App where

import Prelude

import Effect (Effect)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (input)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


type State = Unit


type Action = Unit


component :: forall query input output m. H.Component query input output m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
  }
  where
    render :: forall slots. State -> H.ComponentHTML Action slots m
    render _ =
      HH.div_
        [ HH.h1_ [ HH.text "Hello, Halogen!" ]
        , HH.button
            [ HE.onClick \_ -> unit
            ]
            [ HH.text "Click me" ]
        ]