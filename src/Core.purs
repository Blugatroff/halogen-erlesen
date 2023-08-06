module Select.Core
  ( AllowCustom(..)
  , CreateOptionRenderDescription
  , DropdownRenderDescription(..)
  , InFocus(..)
  , Input
  , InputRenderDescription
  , OptionRenderDescription
  , OptionState(..)
  , Output
  , RenderDescription
  , SelectBehaviour
  , SelectState(..)
  , Dom
  , component
  , symbol
  ) where

import Prelude

import Type.Prelude (Proxy(..))

import Control.Monad.State (class MonadState)

import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe as Maybe

import Data.Generic.Rep (class Generic)
import Data.Traversable (foldMap)
import Data.Show.Generic (genericShow)

import Halogen as H
import Halogen.HTML (HTML)
import Halogen.Query.Event as HQE

import Web.DOM as WD
import Web.DOM.Element as WDE
import Web.UIEvent.FocusEvent as WUF
import Web.UIEvent.KeyboardEvent as WUK
import Web.UIEvent.MouseEvent as WUM
import Web.Event.Event as WEE
import Web.HTML.HTMLDocument as WHHD

symbol = Proxy :: Proxy "Select"

type Input option =
  { options :: Array option
  , selected :: Maybe option
  }

type Output option = Maybe option

type Dom m =
  { getDocument :: m WHHD.HTMLDocument
  , contains :: WD.Node -> WD.Node -> m Boolean
  , preventDefault :: WEE.Event -> m Unit
  , stopPropagation :: WEE.Event -> m Unit
  , scrollIntoView :: WD.Element -> m Unit
  }

type SelectBehaviour option =
  { optionLabel :: option -> String
  , allowCustom :: AllowCustom
  , search :: String -> Array option -> Array option
  , render :: forall w. forall a. RenderDescription option a -> HTML w a
  , parseCustom :: String -> Maybe option
  }

data AllowCustom = AllowCustom | ForbidCustom

derive instance eqAllowCustom :: Eq AllowCustom

type RenderDescription option a =
  { selected :: Maybe option
  , value :: String
  , state :: SelectState
  , onClick :: WUM.MouseEvent -> a
  , onKeyDown :: WUK.KeyboardEvent -> a
  , onFocusOut :: (WUF.FocusEvent -> a)
  , label :: H.RefLabel
  , onCancel :: (Maybe WUM.MouseEvent) -> a
  , dropdown :: DropdownRenderDescription option a
  , input :: InputRenderDescription a
  }

data SelectState = Opened | Filtered | Closed

derive instance eqSelectState :: Eq SelectState
derive instance genericSelectState :: Generic SelectState _
instance showSelectState :: Show SelectState where
  show = genericShow

type InputRenderDescription a =
  { label :: H.RefLabel
  , onValueInput :: String -> a
  }

data DropdownRenderDescription option a
  = DontShow
  | ShowNoOptionsOption
  | ShowOptionsAndCreateOption (Array (OptionRenderDescription option a)) (CreateOptionRenderDescription a)
  | ShowOptions (Array (OptionRenderDescription option a))

type OptionRenderDescription option a =
  { onClick :: a
  , onMouseEnter :: a
  , state :: OptionState
  , label :: H.RefLabel
  , option :: option
  }

type CreateOptionRenderDescription a =
  { onClick :: a
  , onMouseEnter :: a
  , state :: OptionState
  , label :: H.RefLabel
  , value :: String
  }

data Action option
  = Initialize
  | ReceiveInput (Input option)
  | OnClick WUM.MouseEvent
  | OnDocumentClick WEE.Event
  | OnOptionClicked option
  | OnCreateOptionClicked option
  | PreventDefaultOnKeyDown (Array String) WUK.KeyboardEvent
  | OnKeyDown WUK.KeyboardEvent
  | SetValue String
  | OnOptionMouseEnter (InFocus option)
  | OnCancel (Maybe WUM.MouseEvent)
  | OnFocusOut WUF.FocusEvent

data OptionState = OptionHovered | OptionSelected | OptionIdle

data MoveFocus = FocusForward | FocusBackward

derive instance genericMoveFocus :: Generic MoveFocus _
instance showMoveFocus :: Show MoveFocus where
  show = genericShow

data InFocus option = OptionInFocus Int | CreateInFocus option | NothingInFocus

derive instance genericInFocus :: Generic (InFocus option) _
derive instance eqInFocus :: Eq option => Eq (InFocus option)
instance showInFocus :: Show option => Show (InFocus option) where
  show = genericShow

type State option =
  { options :: Array option
  , state :: SelectState
  , value :: String
  , filteredOptions :: Array option
  , inFocus :: InFocus option
  , selected :: Maybe option
  }

component :: forall m option query. Show option => Eq option => Monad m => Dom m -> SelectBehaviour option -> H.Component query (Input option) (Output option) m
component dom behaviour@{ optionLabel } = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { finalize = Nothing
      , initialize = Just Initialize
      , handleAction = handleAction
      , receive = Just <<< ReceiveInput
      }
  }
  where
  initialState :: Input option -> State option
  initialState { options, selected } =
    { options
    , state: Closed
    , value: maybe "" optionLabel selected
    , filteredOptions: options
    , inFocus: OptionInFocus 0
    , selected
    }

  handleAction :: Action option -> H.HalogenM (State option) (Action option) () (Output option) m Unit
  handleAction = case _ of
    Initialize -> do
      document <- H.lift dom.getDocument
      let emitter = HQE.eventListener (WEE.EventType "click") (WHHD.toEventTarget document) (Just <<< OnDocumentClick)
      void $ H.subscribe emitter
    ReceiveInput { options, selected } -> do
      prev <- H.get
      when (prev.options /= options || selected /= prev.selected) do
        H.modify_ \state -> state { options = options, selected = selected, value = maybe "" optionLabel selected }
    OnClick _ -> do
      state <- H.gets _.state
      case state of
        Closed -> H.modify_ \state -> state { state = Opened }
        Opened -> pure unit
        Filtered -> pure unit
    OnCreateOptionClicked custom -> do
      H.modify_ \state -> state { selected = Just custom }
      makeChoice
    OnOptionClicked option -> do
      H.modify_ \state -> state { selected = Just option }
      makeChoice
    OnDocumentClick event ->
      WEE.target event >>= WDE.fromEventTarget # foldMap \target -> do
        H.getRef selectRefLabel >>= foldMap \select -> do
          withinSelect <- H.lift $ dom.contains (WDE.toNode select) (WDE.toNode target)
          when (not withinSelect) $ abort
    OnKeyDown event -> do
      let key = WUK.key event
      when (Array.any (eq key) [ "ArrowUp", "ArrowDown" ]) do
        H.lift $ dom.preventDefault $ WUK.toEvent event
      { state } <- H.get
      case key of
        "Enter" -> do
          case state of
            Opened -> makeChoice
            Filtered -> makeChoice
            Closed -> do
              H.modify_ \state -> state { state = Opened }
        "Escape" -> do
          case state of
            Opened -> abort
            Filtered -> abort
            _ -> pure unit
        "ArrowDown" -> do
          case state of
            Closed -> do
              H.modify_ \state -> state { state = Opened }
              moveFocus FocusForward
            _ -> do
              moveFocus FocusForward
        "ArrowUp" -> do
          case state of
            Closed -> do
              H.modify_ \state -> state { state = Opened }
              moveFocus FocusBackward
            _ -> do
              moveFocus FocusBackward
        _ -> pure unit
    PreventDefaultOnKeyDown preventedKeys event -> do
      let key = WUK.key event
      when (Array.any (eq key) preventedKeys) do
        H.lift $ dom.preventDefault $ WUK.toEvent event
    SetValue value -> do
      H.modify_ \state -> state { value = value }
      doFilter
    OnOptionMouseEnter inFocus -> do
      H.modify_ \state -> state { inFocus = inFocus }
    OnCancel event -> do
      foldMap (H.lift <<< dom.stopPropagation <<< WUM.toEvent) event
      H.modify_ \state -> state { value = "", state = Closed, selected = Nothing }
      H.raise Nothing
    OnFocusOut e -> do
      WUF.relatedTarget e >>= WDE.fromEventTarget # foldMap \element -> do
        H.getRef selectRefLabel >>= foldMap \select -> do
          withinTheSelect <- H.lift $ dom.contains (WDE.toNode select) (WDE.toNode element)
          when (not withinTheSelect) abort

  abort = do
    state <- H.get
    if state.state == Closed then pure unit
    else case state.value of
      "" -> do
        H.modify_ \state -> state { state = Closed, selected = Nothing }
        H.raise Nothing
      _ -> do
        H.modify_ \state -> state
          { state = Closed
          , value = maybe "" optionLabel state.selected
          }

  doFilter = H.get >>= \state ->
    let
      filtered = behaviour.search state.value state.options
      inFocus = case filtered of
        [] -> case behaviour.parseCustom state.value of
          Just custom -> CreateInFocus custom
          Nothing -> NothingInFocus
        options -> case state.inFocus of
          NothingInFocus -> NothingInFocus
          CreateInFocus custom -> CreateInFocus custom
          OptionInFocus index -> case Array.index options index of
            Just _ -> OptionInFocus index
            Nothing -> OptionInFocus 0
    in
      H.modify_ \state -> case state.value of
        "" -> state { state = Opened, filteredOptions = [] }
        _ -> state { state = Filtered, filteredOptions = filtered, inFocus = inFocus }

  makeChoice :: forall i. H.HalogenM (State option) i () (Maybe option) m Unit
  makeChoice = H.gets _.inFocus >>= case _ of
    NothingInFocus -> pure unit
    CreateInFocus option -> case behaviour.allowCustom of
      AllowCustom -> do
        moveFocusToInput
        H.modify_ \state -> state { state = Closed }
        H.raise $ Just option
      ForbidCustom -> pure unit

    OptionInFocus index -> do
      moveFocusToInput
      currentOptions <- currentOptions
      let selected = Array.index currentOptions index
      case selected of
        Nothing -> pure unit
        Just selected -> do
          H.modify_ \state -> state
            { value = optionLabel selected
            , state = Closed
            }
          H.raise $ Just selected

  moveFocusToInput :: forall i o. H.HalogenM (State option) i () o m Unit
  moveFocusToInput = do
    H.getRef inputRefLabel >>= foldMap \input -> do
      H.lift $ dom.scrollIntoView input

  moveFocusToOption :: forall i o. Int -> H.HalogenM (State option) i () o m Unit
  moveFocusToOption index = do
    H.getRef (optionRefLabel index) >>= foldMap \element -> do
      H.lift $ dom.scrollIntoView element
    H.modify_ \state -> state { inFocus = OptionInFocus index }

  moveFocusToCreate = do
    H.getRef createOptionRefLabel >>= foldMap \element -> do
      H.lift $ dom.scrollIntoView element
    H.modify_ \state -> case behaviour.parseCustom state.value of
      Nothing -> state
      Just custom -> state { inFocus = CreateInFocus custom }

  currentOptions :: forall hm. MonadState (State option) hm => hm (Array option)
  currentOptions = H.get <#> \state ->
    if state.state == Filtered then state.filteredOptions else state.options

  moveFocus toThere = do
    { inFocus } <- H.get
    currentOptions <- currentOptions
    if Array.null currentOptions then pure unit
    else do
      case inFocus, toThere of
        NothingInFocus, FocusForward -> moveFocusToOption 0
        NothingInFocus, FocusBackward -> if behaviour.allowCustom == AllowCustom then moveFocusToCreate else pure unit
        CreateInFocus _, FocusForward -> do
          moveFocusToOption 0
        CreateInFocus _, FocusBackward -> do
          moveFocusToOption (Array.length currentOptions - 1)
        OptionInFocus fromHere, FocusForward -> do
          case fromHere of
            n | n == Array.length currentOptions - 1 -> case behaviour.allowCustom of
              AllowCustom -> moveFocusToCreate
              ForbidCustom -> moveFocusToOption 0
            n -> do
              moveFocusToOption (n + 1)
        OptionInFocus fromHere, FocusBackward -> do
          case fromHere of
            0 -> case behaviour.allowCustom of
              AllowCustom -> moveFocusToCreate
              ForbidCustom -> moveFocusToOption (Array.length currentOptions - 1)
            n -> do
              moveFocusToOption (n - 1)

  selectRefLabel = H.RefLabel "select"
  inputRefLabel = H.RefLabel "input"
  createOptionRefLabel = H.RefLabel "create"
  optionRefLabel index = H.RefLabel $ show (index + 1)

  thereIsNoCorrespondingOption value options = Maybe.isNothing $ Array.find (optionLabel >>> eq value) options

  render :: forall w. State option -> HTML w (Action option)
  render = behaviour.render <<< makeRenderDescription

  makeRenderDescription :: State option -> RenderDescription option (Action option)
  makeRenderDescription { selected, options, filteredOptions, value, state, inFocus } =
    { onClick: OnClick
    , onKeyDown: OnKeyDown
    , label: selectRefLabel
    , dropdown: case state, behaviour.allowCustom of
        Closed, _ -> DontShow
        _, ForbidCustom | Array.null currentOptions -> ShowNoOptionsOption
        _, ForbidCustom -> ShowOptions $ Array.mapWithIndex makeOptionRenderDescription currentOptions
        _, AllowCustom | thereIsNoCorrespondingOption value currentOptions -> case createOptionRenderDescription of
          Just createOptionRenderDescription -> ShowOptionsAndCreateOption (Array.mapWithIndex makeOptionRenderDescription currentOptions) createOptionRenderDescription
          Nothing -> case currentOptions of
            [] -> ShowNoOptionsOption
            currentOptions -> ShowOptions $ Array.mapWithIndex makeOptionRenderDescription currentOptions
        _, AllowCustom -> ShowOptions $ Array.mapWithIndex makeOptionRenderDescription currentOptions
    , value
    , selected
    , onFocusOut: OnFocusOut
    , onCancel: OnCancel
    , state
    , input: { onValueInput: SetValue, label: inputRefLabel }
    }
    where
    currentOptions = case state of
      Filtered -> filteredOptions
      Opened -> options
      _ -> []

    makeOptionRenderDescription :: Int -> option -> OptionRenderDescription option (Action option)
    makeOptionRenderDescription i option = { onClick, onMouseEnter, state, label, option }
      where
      onClick = OnOptionClicked option
      onMouseEnter = OnOptionMouseEnter $ OptionInFocus i
      label = optionRefLabel i
      state = case inFocus, selected of
        NothingInFocus, _ -> OptionIdle
        CreateInFocus _, Just selected | selected == option -> OptionSelected
        CreateInFocus _, Just _ -> OptionIdle
        CreateInFocus _, Nothing -> OptionIdle
        OptionInFocus _, Just selected | selected == option -> OptionSelected
        OptionInFocus inFocus, _ | inFocus == i -> OptionHovered
        OptionInFocus _, _ -> OptionIdle

    createOptionRenderDescription = case behaviour.parseCustom value of
      Nothing -> Nothing
      Just custom -> do
        let
          state = case inFocus, selected of
            NothingInFocus, _ -> OptionIdle
            CreateInFocus _, Just c | c == custom -> OptionSelected
            CreateInFocus _, Just _ -> OptionHovered
            CreateInFocus _, Nothing -> OptionHovered
            OptionInFocus _, Just c | c == custom -> OptionSelected
            OptionInFocus _, Just _ -> OptionIdle
            OptionInFocus _, Nothing -> OptionIdle
          onMouseEnter = OnOptionMouseEnter (CreateInFocus custom)
        Just { onClick: OnCreateOptionClicked custom, onMouseEnter, state, label: createOptionRefLabel, value }
