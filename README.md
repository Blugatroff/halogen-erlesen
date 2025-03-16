# halogen-erlesen

A simple select component with dropdown for [Halogen](https://github.com/purescript-halogen/purescript-halogen).

## Usage
You need to use the mkComponent function to create a usable Halogen Component.
It accepts a `Dom` and a `Selectbehaviour`.
- The `Dom` argument proves the select with functions for manipulating and queriering the Dom.
  It is parameterized by the Monad in which to run, this must match the monad in which you 
  interpret the component later.

  if you just want to run your component in `Aff` then you can use the provided `domAff` from `Select.Default`.
- The `SelectBehaviour` specifies how the select should be rendered, how to parse the user input, 
whether to custom (ones not present in the provided options) inputs, how to search the available options
and how to get a string representation of an `option`.

```haskell
mkComponent
  :: forall m option query
   . Dom m
  -> SelectBehaviour option
  -> H.Component query (Input option) (Output option) m
```
Do not call this function on every render, do it once for every variant of the select you need.

Look at the [Examples](./examples/src/Main.purs) for minimal examples.

Run the examples:
```bash
spago build
python -m http.server 8000 &
firefox http://127.0.0.1:8000/examples/
```

## Important Types

The select accepts an array of options and the currently selected option as inputs:
```haskell
type Input input option =
  { options :: Array option
  , selected :: Maybe option
  , input :: input
  }
```

Whenever the user of the select makes a choice you get a `Maybe option`.
```haskell
data Output option = Selected (Maybe option) | Blurred WUF.FocusEvent
```


```haskell
type SelectBehaviour option =
  { optionLabel :: option -> String
  , allowCustom :: AllowCustom
  , search :: String -> Array option -> Array option
  , render :: forall w. forall a. RenderDescription option a -> HTML w a
  , parseCustom :: String -> Maybe option
  }

data AllowCustom = AllowCustom | ForbidCustom
```

### Render Descriptions
```haskell
type RenderDescription option a =
  -- | the currently selected option if any
  { selected :: Maybe option
  -- | The content of your input
  , value :: String
  -- | whether the state is Closed, Opened, or Filtered
  , state :: SelectState
  -- | This should be attached to the root element of your select.
  , onClick :: WUM.MouseEvent -> a
  -- | This should be attached to the root element of your select.
  , onKeyDown :: WUK.KeyboardEvent -> a
  -- | This should be attached to the root element of your select.
  , onFocusOut :: (WUF.FocusEvent -> a)
  -- | This should be attached to the root element of your select.
  , onBlur :: (WUF.FocusEvent -> a)
  -- | This should be attached to the root element of your select.
  , label :: H.RefLabel
  -- | Fire this (perhaps from a mouseevent) to cancel the selection.
  , onCancel :: (Maybe WUM.MouseEvent) -> a
  , dropdown :: DropdownRenderDescription option a
  , input :: InputRenderDescription a
  }

data SelectState = Opened | Filtered | Closed

type InputRenderDescription a =
  { label :: H.RefLabel
  , onValueInput :: String -> a
  }

data DropdownRenderDescription option a
  = DontShow
  | ShowNoOptionsOption
  | ShowOptionsAndCreateOption (Array (OptionRenderDescription option a)) (OptionRenderDescription String a)
  | ShowOptions (Array (OptionRenderDescription option a))

type OptionRenderDescription option a =
  { onClick :: a
  , onMouseEnter :: a
  , state :: OptionState
  , label :: H.RefLabel
  , value :: option
  }
```
