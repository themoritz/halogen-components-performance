module Main where

import Prelude

import Halogen (ChildF, Component, HalogenEffects, ParentDSL, ParentHTML, ParentState, modify, parentComponent, parentState, runUI)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Control.Monad.Eff (Eff)
import Data.Array (range)
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(Nothing))
import Halogen.Util (awaitBody, runHalogenAff)
import Ticker (TickInput, TickState(TickState), ticker)

data Input a
  = IncSize a

type State =
  { width :: Int
  , height :: Int
  }

initialState :: State
initialState =
  { width: 30
  , height: 10
  }

data TickSlot = TickSlot Int Int

derive instance eqTickSlot :: Eq TickSlot
derive instance ordTickSlot :: Ord TickSlot

type StateP g = ParentState State TickState Input TickInput g TickSlot
type InputP = Coproduct Input (ChildF TickSlot TickInput)

--

ui :: forall g. Functor g => Component (StateP g) InputP g
ui = parentComponent { render, eval, peek: Nothing }

render :: forall g. Functor g => State -> ParentHTML TickState Input TickInput g TickSlot
render st = H.div_
    [ H.button
      [ E.onClick (E.input_ IncSize) ]
      [ H.text "Increase size" ]
    , H.table_ $ row <$> range 0 st.height
    ]
  where
    row y = H.tr_ $ cell y <$> range 0 st.width
    cell y x = H.td_ [ H.slot (TickSlot x y) \_ -> { component: ticker, initialState: TickState (x + y) }]

eval :: forall g. (Functor g) => Input ~> ParentDSL State TickState Input TickInput g TickSlot
eval (IncSize next) = do
  modify (\r -> { width: r.width, height: r.height+10 })
  pure next

--

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff $ do
  body <- awaitBody
  runUI ui (parentState initialState) body
