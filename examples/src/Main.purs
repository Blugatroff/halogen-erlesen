module Main where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.String.Utils as SU
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HHE
import Halogen.HTML.Properties as HHP
import Halogen.VDom.Driver as HVD
import Select.Core (AllowCustom(..), DropdownRenderDescription(..), OptionRenderDescription, OptionState(..), Output(..), RenderDescription)
import Select.Core as Select
import Select.Default as SelectDef

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  _halogenIO <- HVD.runUI component unit body
  pure unit

behaviour :: forall input. Select.SelectBehaviour input String
behaviour = { allowCustom: AllowCustom, optionLabel: identity, parseCustom: Just, render, search }
  where
  search query options = Array.filter (SU.startsWith query) options

  render :: forall w a. input -> RenderDescription String a -> HH.HTML w a
  render _ spec = HH.div
    [ HHE.onClick spec.onClick
    , HHE.onKeyDown spec.onKeyDown
    , HHE.onFocusOut spec.onFocusOut
    , HHE.onBlur spec.onBlur
    , HHP.ref spec.label
    ]
    [ HH.div []
        [ HH.input [ HHP.value spec.value, HHP.ref spec.input.label, HHE.onValueInput spec.input.onValueInput ]
        , HH.button [ HHE.onClick (Just >>> spec.onCancel) ] [ HH.text "X" ]
        ]
    , case spec.dropdown of
        DontShow -> HH.span_ []
        ShowNoOptionsOption -> HH.text "No Options available"
        ShowOptions options -> HH.ul [] (map renderOption options)
        ShowOptionsAndCreateOption options createOption -> HH.ul [] (append (map renderOption options) [ renderOption createOption ])
    ]

  renderOption :: forall w a. OptionRenderDescription String a -> HH.HTML w a
  renderOption option = HH.li
    [ HHE.onClick (const option.onClick), HHE.onMouseEnter (const option.onMouseEnter), HHP.ref option.label, HHP.style style ]
    [ HH.text option.value ]
    where
    style = case option.state of
      OptionIdle -> ""
      OptionHovered -> "background-color: blue"
      OptionSelected -> "background-color: red"

selectComponent = Select.mkComponent SelectDef.domAff behaviour

component :: forall query input. H.Component query input Void Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }
  where
  initialState _ = { selected: Nothing, outputs: [] }

  handleAction = case _ of
    Selected option -> do
      now <- JSDate.toTimeString <$> H.liftEffect JSDate.now
      void $ H.fork do
        H.lift $ Aff.delay (Aff.Milliseconds 10_000.0)
        H.modify_ \state -> state { outputs = Array.filter (\{ time } -> time /= now) state.outputs }
      H.modify_ \state -> state { selected = option, outputs = { time: now, output: option } : state.outputs }
    Blurred _ -> pure unit

  render state = HH.div []
    [ HH.slot Select.symbol unit selectComponent { options: [ "Hello", "There" ], selected: state.selected, input: unit } identity
    , HH.div [] do
        { time, output } <- state.outputs
        pure $ HH.p [] [ HH.text (time <> " " <> show output) ]
    ]
