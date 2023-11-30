% Declarative GTK Programming
% Jens Petersen (@juhp)
% <small><petersen@redhat.com></small>\
    \
    ![GNOME Asia Summit 2023](2023GA-webbanner-sm.png)

# Brief background

- Elm
- React
- FRP
- virtual dom

# Motivation
- clear separation of concerns: Model vs View vs UI
- restrict/control flow of events (messages) for better consistency
- rendering update optimisations (vdom diff's)

# Tutorial Talk Overview

- Declarative/Reactive UI pattern
- Elm examples
- GTK examples

"vdom" reactive type layers over gtk

cf FRP: _Functional Reactive Programming_

# Elm

A functional language that compiles to JavaScript.

Primarily designed to make web application and components.

<https://elm-lang.org/>

Like Haskell:

- compiled
- static types
- type checked

<https://guide.elm-lang.org/>

-    No runtime errors in practice.
-    Friendly error messages.
-    Reliable refactoring.
-    Automatically enforced semantic versioning for all Elm packages.

<https://guide.elm-lang.org/core_language>

# The Elm Architecture

Types:

- Message
- Model (or app State)
- HTML

Logic (pure functions):

- `view`
- `update`

Runtime

- executes the declarative application

# Very brief intro to Elm data types

Specific types written with a capital letter: `Int`, `Bool`, `String`, etc.

## Product types:

- products

```elm
type MyInt Int
```

```elm
type Pair a b

type Program flags model msg
```

these lowercase identifiers are type variables standing for arbitrary or generic types.

- records

```
{ok : Bool, title : String}
```

## Sum types

- enums

```elm
type Okay = Yes | No
```

```elm
type Color = Red | Yellow | Blue
```

- Algebraic Data Types (ADT)

```elm
type Source = File FilePath | URL URI | Error String | Retry
```

## Functions

```elm
add : Int -> Int -> Int
add x y = x + y
```

```elm
take : Int -> String -> String`
```

```elm
combine : Bool -> Int -> Double -> Double
```

<https://guide.elm-lang.org/core_language>

# Elm example 1: counter

```elm
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type alias Model = Int

init : Model
init = 0

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment -> model + 1
    Decrement -> model - 1

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]

main =
  Browser.sandbox { init = init, update = update, view = view }
```

<https://elm-lang.org/examples/buttons>

## Elm `sandbox`

A “sandboxed” program that cannot communicate with the outside world.

```elm
sandbox :
    { init : model
    , view : model -> Html msg
    , update : msg -> model -> model
    }
    -> Program () model msg
```

<https://package.elm-lang.org/packages/elm/browser/latest/Browser#sandbox>

## Exercise 1

Add a reset button to our counter.

Shows how simple to refactor

# State/Model type

The Model (or State) contains the dynamic data of the application (program)

examples:

- Bool for a switch
- Int for a number
- String for some text

Typically a product or algebraic data type (ADT)

Some more examples soon

# Basic Declarative UI runtime

`init ==> view =msg=> update ==> view =msg=> update ==> view ==> ...`

# Elm example 2: text field

<https://guide.elm-lang.org/architecture/text_fields>

<https://elm-lang.org/examples/text-fields>

## Exercise 2
Show length using `String.length`

# Elm example 3: form

<https://guide.elm-lang.org/architecture/forms>

<https://elm-lang.org/examples/forms>

## Exercise 3
Check that password is at least 8 characters.

# Error handling

`type Maybe a = Just a | Nothing`

- <https://guide.elm-lang.org/error_handling/maybe>

> I call it my billion-dollar mistake. It was the invention of the null reference in 1965. At that time, I was designing the first comprehensive type system for references in an object oriented language (ALGOL W). My goal was to ensure that all use of references should be absolutely safe, with checking performed automatically by the compiler. But I couldn't resist the temptation to put in a null reference, simply because it was so easy to implement. This has led to innumerable errors, vulnerabilities, and system crashes, which have probably caused a billion dollars of pain and damage in the last forty years.

  --- Tony Hoare

- <https://guide.elm-lang.org/error_handling/result>

# Elm Browser module `element`

```elm
element :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Program flags model msg
```

<https://package.elm-lang.org/packages/elm/browser/latest/Browser#element>

<https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#Cmd>

# Elm example 4: random dice

<https://guide.elm-lang.org/effects/random>

<https://elm-lang.org/examples/numbers>

# So can do The Elm Architecture in GTK?

Yes

# Haskell GTK bindings

- gtk2hs

- haskell-gi
  - [gi-gtk](https://hackage.haskell.org/package/gi-gtk) (gtk3 and gtk4)
  - by Iñaki García Etxebarria

## gi-gtk hello

```haskell
main :: IO ()
main = do
  Gtk.init Nothing

  win <- new Gtk.Window [ #title := "Hi there" ]

  on win #destroy Gtk.mainQuit

  button <- new Gtk.Button [ #label := "Click me" ]

  on button #clicked $
    set
      button
      [ #sensitive := False
      , #label := "Thanks for clicking me"
      ]

  #add win button

  #showAll win

  Gtk.main
```

# Haskell Declarative gtk

 - [gi-gtk-declarative]()
   - gi-gtk-declarative-app-simple
   - by Oskar Wickström
 - built on top of gtk3 and Haskell gi-gtk bindings

<https://owickstrom.github.io/gi-gtk-declarative/>

# gi-gtk-declarative examples

<https://github.com/owickstrom/gi-gtk-declarative/blob/master/examples/>

- Functor
- Exit
- AddBoxes

# gi-gtk-declarative-app-simple

<https://owickstrom.github.io/gi-gtk-declarative/app-simple/>

```haskell
data App window state event =
  App
    { update       :: state -> event -> Transition state event
    , view         :: state -> AppView window event
    , inputs       :: [Producer event IO ()]
    , initialState :: state
    }
```

<https://github.com/owickstrom/gi-gtk-declarative/blob/master/gi-gtk-declarative-app-simple/src/GI/Gtk/Declarative/App/Simple.hs>

# Example gi-gtk-declarative apps

- [compare-fonts](https://github.com/juhp/compare-fonts)
  - port of python fonts-compare tool
- <https://github.com/owickstrom/komposition> (video editor)

# Rust Relm4

- Rust
- originally based on Relm (gtk3)
- Relm4 is based on gtk4 and gtk-rs bindings

<https://github.com/Relm4/Relm4/tree/main/examples>

# Nim Owl Kettle

Nim declarative GTK library

<https://github.com/can-lehmann/owlkettle>

<https://can-lehmann.github.io/owlkettle/README.html>

<https://github.com/can-lehmann/owlkettle/tree/main/examples>

# Summary

Hope I have convinced you that Declarative UIs are simpler and easier
to maintain, debug, and scale.

- Go through <https://guide.elm-lang.org>
- Try <https://owickstrom.github.io/gi-gtk-declarative/>
- Play with <https://can-lehmann.github.io/owlkettle/README.html>
  and <https://relm4.org/book/stable/>

Many other reactive/declarative options also exist,
but in this tutorial talk I focused on the purest kinds.

Checkout also:

- <https://github.com/reflex-frp>
- <https://github.com/fjvallarino/monomer>
- <https://github.com/utkarshkukreti/purescript-hedwig>
- Functional Reactive Programming (FRP)

Like Haskell, even if you don't continue Declarative UI programming,
learning will still make you a better application programmer.
