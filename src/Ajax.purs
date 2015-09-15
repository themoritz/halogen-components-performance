module Ajax where

import Prelude

import Halogen
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Events as E

data AjaxState = AjaxState String

data AjaxInput a
  = GetAjax a

ajax :: forall g p. (Functor g) => Component AjaxState AjaxInput g p
ajax = component render eval
  where
