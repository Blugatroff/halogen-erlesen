module Select.Default where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as Aff
import Select.Core as Select
import Web.DOM.Element as WDE
import Web.DOM.Node as WDN
import Web.Event.Event as WEE
import Web.HTML as WH
import Web.HTML.Window as WHW

foreign import scrollIntoView :: WDE.Element -> Effect Unit

domAff :: Select.Dom Aff
domAff = Select.liftDom Aff.liftEffect domEffect

domEffect :: Select.Dom Effect
domEffect = Select.Dom
  { getDocument: WHW.document =<< WH.window
  , contains: WDN.contains
  , preventDefault: WEE.preventDefault
  , scrollIntoView: scrollIntoView
  , stopPropagation: WEE.stopPropagation
  }

