module Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Plus (Plus)

import Data.Tuple
import Data.Array (range)
import Data.Const (Const())
import Data.NaturalTransformation (Natural())
import Data.Functor.Coproduct (Coproduct())
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Data.Void (Void())

import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Events as E

import Ticker (TickState(..), TickInput(..), ticker)

data Input a
  = IncSize a

type State = { width :: Int, height :: Int }

initialState :: State
initialState = { width: 10, height: 10 }

data TickP = TickP Int Int

instance eqTickP :: Eq TickP where
  eq (TickP x y) (TickP i j) = eq x i && eq y j

instance ordTickP :: Ord TickP where
  compare (TickP x y) (TickP i j) = compare (Tuple x y) (Tuple i j)

--

uiContainer :: forall g p. (Functor g) => ParentComponent State TickState Input TickInput g (Const Void) TickP p
uiContainer = component render eval

render :: Render State Input TickP
render st = H.div_
    [ H.button
      [ E.onClick (E.input_ IncSize) ]
      [ H.text "Increase size" ]
    , H.table_ $ row <$> range 0 st.height
    ]
  where
    row y = H.tr_ $ cell y <$> range 0 st.width
    cell y x = H.td_ [ H.Placeholder (TickP x y) ]

eval :: forall g. (Functor g) => Eval Input State Input g
eval (IncSize next) = do
  modify (\r -> { width: r.width+10, height: r.height+10 })
  pure next

--

ui :: forall g p. (Monad g, Plus g) => InstalledComponent State TickState Input TickInput g (Const Void) TickP p
ui = install uiContainer go
  where
  go :: TickP -> ComponentState TickState TickInput g p
  go (TickP x y) = Tuple ticker (TickState (x+y))

--

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui (installedState initialState)
  appendToBody app.node
