module Ticker where

import Prelude
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen (Component, ComponentDSL, ComponentHTML, component, modify)

data TickState = TickState Int

data TickInput a
  = Tick a

ticker :: forall g. Functor g => Component TickState TickInput g
ticker = component
  { render
  , eval
  }

render :: TickState -> ComponentHTML TickInput
render (TickState n) = H.div_
  [ H.text (show n)
  , H.br_
  , H.button [ E.onClick (E.input_ Tick) ] [ H.text "T" ]
  ]

eval :: forall g. Functor g => TickInput ~> ComponentDSL TickState TickInput g
eval (Tick next) = do
  modify (\(TickState n) -> TickState (n + 1))
  pure next
