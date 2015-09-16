module Ticker where

import Prelude

import Data.Functor (($>))

import Halogen
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Events as E

data TickState = TickState Int

data TickInput a
  = Tick a

ticker :: forall g p. (Functor g) => Component TickState TickInput g p
ticker = component render eval

render :: forall p. Render TickState TickInput p
render (TickState n) = H.div_
  [ H.text (show n)
  , H.button [ E.onClick (E.input_ Tick) ] [ H.text "Tick" ]
  ]

eval :: forall g. (Functor g) => Eval TickInput TickState TickInput g
eval (Tick next) = do
  modify (\(TickState n) -> TickState (n + 1))
  pure next
