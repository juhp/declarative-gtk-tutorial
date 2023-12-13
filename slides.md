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

<br/>

_Declare what our application should do in pure functions, which are then executed by an UI runtime_

# Tutorial Talk Overview

- Declarative/Reactive UI pattern
- Elm examples
- GTK examples

"vdom" reactive type layers over gtk

<br/>

- Slides: <https://petersen.fedorapeople.org/talks> -> [gnome-asia-2023-declaritive-gtk.html](https://petersen.fedorapeople.org/talks/gnome-asia-2023-declaritive-gtk.html)
  - (source: <https://github.com/juhp/declarative-gtk-tutorial/>)

# Elm

A pure functional language that compiles to JavaScript.

Primarily designed to make web application and components.

<https://elm-lang.org/>

Like Haskell:

- compiled
- static types
- type checked

<https://guide.elm-lang.org/>

- no runtime errors in practice
- friendly error messages
- reliable refactoring

<https://guide.elm-lang.org/core_language>

# The Elm Architecture

Types:

- Message
- Model (or app State)
- HTML

Logic (pure functions):

- `view`
- `update`
- `init`

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

- enumerations

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
take : Int -> String -> String
```

```elm
combine : Bool -> Int -> Double -> Double
```

<https://guide.elm-lang.org/core_language>

# State/Model type

The Model (or State) contains the dynamic data of the application (program)

examples:

- Bool for a switch
- Int for a number
- String for some text

Typically a product or algebraic data type (ADT)

eg

```elm
type Model = {name : String, address : String}
```
or
```elm
type Model = User String Int | AnonymousUser
```

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

## Exercise 1

Add a reset button to our counter.

Shows how simple to refactor

# Elm `sandbox`

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

# Elm sandbox Declarative UI runtime

`init ==> view =msg=> update ==> view =msg=> update ==> view =msg=> ...`

# Elm example 2: text field

- <https://elm-lang.org/examples/text-fields>

```elm
-- https://guide.elm-lang.org/architecture/text_fields.html

import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

type alias Model =
  { content : String
  }

init : Model
init = { content = "" }

type Msg = Change String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Text to reverse", value model.content, onInput Change ] []
    , div [] [ text (String.reverse model.content) ]
    ]

main = Browser.sandbox { init = init, update = update, view = view }
```
## Exercise 2
Show length using `String.length`

# Elm example 3: form

<https://elm-lang.org/examples/forms>

```elm
-- https://guide.elm-lang.org/architecture/forms.html

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }

init : Model
init = Model "" "" ""

type Msg
  = Name String
  | Password String
  | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html msg
viewValidation model =
  if model.password == model.passwordAgain then
    div [ style "color" "green" ] [ text "OK" ]
  else
    div [ style "color" "red" ] [ text "Passwords do not match!" ]

main =
  Browser.sandbox { init = init, update = update, view = view }
```

## Exercise 3
Check that password is at least 8 characters.


<!--
# Error handling

`type Maybe a = Just a | Nothing`

- <https://guide.elm-lang.org/error_handling/maybe>

> I call it my billion-dollar mistake. It was the invention of the null reference in 1965. At that time, I was designing the first comprehensive type system for references in an object oriented language (ALGOL W). My goal was to ensure that all use of references should be absolutely safe, with checking performed automatically by the compiler. But I couldn't resist the temptation to put in a null reference, simply because it was so easy to implement. This has led to innumerable errors, vulnerabilities, and system crashes, which have probably caused a billion dollars of pain and damage in the last forty years.

  --- Tony Hoare

- <https://guide.elm-lang.org/error_handling/result>
-->

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

```elm
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random

type alias Model =
  { dieFace : Int }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 1
  , Cmd.none
  )

type Msg
  = Roll
  | NewFace Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate NewFace (Random.int 1 6)
      )

    NewFace newFace ->
      ( Model newFace
      , Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (String.fromInt model.dieFace) ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
```

<https://elm-lang.org/examples/numbers>

# Elm element Declarative UI runtime

`init =Cmd=> view =msg=> update =Cmd=> view =msg=> update =Cmd=> view =msg=> ...`


# So can do The Elm Architecture in GTK?

Yes

# Haskell GTK bindings

- gtk2hs

- haskell-gi
  - [gi-gtk](https://hackage.haskell.org/package/gi-gtk) (gtk3 and gtk4)
  - by Iñaki García Etxebarria

# gi-gtk hello

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
 - built on top of the gtk3 Haskell gi-gtk bindings

<https://owickstrom.github.io/gi-gtk-declarative/>

<br/>

<https://hackage.haskell.org/package/gi-gtk-declarative>

# gi-gtk-declarative example 1

## Functor
<https://github.com/owickstrom/gi-gtk-declarative/blob/master/examples/Functor.hs>

```haskell
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Functor where

import           Control.Monad                  ( void )
import           Data.Functor                   ( ($>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           GI.Gtk                         ( Box(..)
                                                , Button(..)
                                                , Label(..)
                                                , Orientation(..)
                                                , Window(..)
                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data ButtonEvent = ButtonClicked

clickyButton :: Text -> Widget ButtonEvent
clickyButton label = widget Button [#label := label, on #clicked ButtonClicked]

data State = State { count :: Integer }

data Event = Incr | Decr | Closed

view' :: State -> AppView Window Event
view' State {..} =
  bin
      Window
      [ #title := "Functor"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 400
      , #heightRequest := 300
      ]
    $ container
        Box
        [#orientation := OrientationVertical]
        [ expandingChild $ widget Label [#label := Text.pack (show count)]
        , BoxChild defaultBoxChildProperties $ container
          Box
          [#orientation := OrientationHorizontal]
          [ expandingChild $ clickyButton "-1" $> Decr
          , expandingChild $ clickyButton "+1" $> Incr
          ]
        ]
 where
  expandingChild =
    BoxChild defaultBoxChildProperties { expand = True, fill = True }

update' :: State -> Event -> Transition State Event
update' State {..} Incr   = Transition (State (count + 1)) (return Nothing)
update' State {..} Decr   = Transition (State (count - 1)) (return Nothing)
update' _          Closed = Exit

main :: IO ()
main = void $ run App { view         = view'
                      , update       = update'
                      , inputs       = []
                      , initialState = State 0
                      }
```

# gi-gtk-declarative example 2

## Exit
<https://github.com/owickstrom/gi-gtk-declarative/blob/master/examples/Exit.hs>

```haskell
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Exit where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( void )
import           Data.Functor                   ( ($>) )
import qualified Data.Text                     as Text

import           GI.Gtk                         ( Button(..)
                                                , Label(..)
                                                , Window(..)
                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple (App(..), AppView,
                                                Transition(..), run)

data State = Running | ExitingIn Int

data Event = ExitApplication | CountDownExit

view' :: State -> AppView Window Event
view' s =
  bin
      Window
      [ #title := "Exit"
      , on #deleteEvent (const (True, ExitApplication))
      , #widthRequest := 400
      , #heightRequest := 300
      ]
    $ case s of
        Running ->
          widget Button [#label := "Exit", on #clicked ExitApplication]
        ExitingIn sec -> widget
          Label
          [#label := ("Exiting in " <> Text.pack (show sec) <> " seconds.")]

countDown :: IO (Maybe Event)
countDown = threadDelay oneSec $> Just CountDownExit
 where
  oneSec :: Int
  oneSec = 1000000

update' :: State -> Event -> Transition State Event
update' Running       ExitApplication = Transition (ExitingIn 3) countDown
update' Running       _               = Transition Running (pure Nothing)
update' (ExitingIn 1) CountDownExit   = Exit
update' (ExitingIn sec) CountDownExit = Transition (ExitingIn (pred sec)) countDown
update' s@ExitingIn{} ExitApplication = Transition s (pure Nothing)

main :: IO ()
main = void $ run App { view         = view'
                      , update       = update'
                      , inputs       = []
                      , initialState = Running
                      }
```

# gi-gtk-declarative example 3

## AddBoxes
<https://github.com/owickstrom/gi-gtk-declarative/blob/master/examples/AddBoxes.hs>

```haskell
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AddBoxes where

import           Control.Monad                  ( void )
import qualified Data.Text                     as Text
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector

import           GI.Gtk                         ( Box(..)
                                                , Button(..)
                                                , Label(..)
                                                , Orientation(..)
                                                , PolicyType(..)
                                                , ScrolledWindow(..)
                                                , Window(..)
                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data Event = AddLeft | AddRight | Closed

data State = State { lefts :: Vector Int, rights :: Vector Int, next :: Int }

addBoxesView :: State -> AppView Window Event
addBoxesView State {..} =
  bin
      Window
      [ #title := "AddBoxes"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 400
      , #heightRequest := 300
      ]
    $ bin
        ScrolledWindow
        [ #hscrollbarPolicy := PolicyTypeAutomatic
        , #vscrollbarPolicy := PolicyTypeNever
        ]
    $ container Box
                [#orientation := OrientationVertical]
                [renderLane AddLeft lefts, renderLane AddRight rights]
 where
  renderLane :: Event -> Vector Int -> BoxChild Event
  renderLane onClick children =
    BoxChild defaultBoxChildProperties { padding = 10 } $ container
      Box
      []
      (             BoxChild defaultBoxChildProperties { padding = 10 } btn
      `Vector.cons` Vector.map
                      ( BoxChild defaultBoxChildProperties { padding = 5 }
                      . renderChild
                      )
                      children
      )
    where btn = widget Button [#label := "Add", on #clicked onClick]
  renderChild :: Int -> Widget Event
  renderChild n = widget Label [#label := Text.pack (show n)]

update' :: State -> Event -> Transition State Event
update' state@State {..} AddLeft = Transition
  state { lefts = lefts `Vector.snoc` next, next = succ next }
  (return Nothing)
update' state@State {..} AddRight = Transition
  state { rights = rights `Vector.snoc` next, next = succ next }
  (return Nothing)
update' _ Closed = Exit

main :: IO ()
main = void $ run App { view         = addBoxesView
                      , update       = update'
                      , inputs       = []
                      , initialState = State [1] [2] 3
                      }
```

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

<https://hackage.haskell.org/package/gi-gtk-declarative-app-simple-0.7.1/docs/GI-Gtk-Declarative-App-Simple.html>

# gi-gtk-declarative apps

## compare-fonts

- <https://github.com/juhp/compare-fonts>

![a PoC port of the fonts-compare tool (python)](https://github.com/juhp/compare-fonts/blob/main/compare-fonts.png?raw=true)

# gi-gtk-declarative apps (cont)

## komposition

a video editor

<https://github.com/owickstrom/komposition>

![](https://github.com/owickstrom/komposition/blob/master/docs/src/screenshot.png?raw=true)

<https://wickstrom.tech/2018-10-26-writing-a-screencast-video-editor-in-haskell.html>

# Rust Relm4

- Rust
- originally based on Relm (gtk3)
- Relm4 is based on gtk4 and gtk-rs bindings

<https://github.com/Relm4/Relm4>

# Relm4 example 1

<https://github.com/Relm4/Relm4/blob/main/examples/simple_manual.rs>

```rust
use gtk::glib::clone;
use gtk::prelude::{BoxExt, ButtonExt, GtkWindowExt};
use relm4::{gtk, ComponentParts, ComponentSender, RelmApp, RelmWidgetExt, SimpleComponent};

struct AppModel {
    counter: u8,
}

#[derive(Debug)]
enum AppInput {
    Increment,
    Decrement,
}

struct AppWidgets {
    label: gtk::Label,
}

impl SimpleComponent for AppModel {

    /// The type of the messages that this component can receive.
    type Input = AppInput;
    /// The type of the messages that this component can send.
    type Output = ();
    /// The type of data with which this component will be initialized.
    type Init = u8;
    /// The root GTK widget that this component will create.
    type Root = gtk::Window;
    /// A data structure that contains the widgets that you will need to update.
    type Widgets = AppWidgets;

    fn init_root() -> Self::Root {
        gtk::Window::builder()
            .title("Simple app")
            .default_width(300)
            .default_height(100)
            .build()
    }

    /// Initialize the UI and model.
    fn init(
        counter: Self::Init,
        window: &Self::Root,
        sender: ComponentSender<Self>,
    ) -> relm4::ComponentParts<Self> {
        let model = AppModel { counter };

        let vbox = gtk::Box::builder()
            .orientation(gtk::Orientation::Vertical)
            .spacing(5)
            .build();

        let inc_button = gtk::Button::with_label("Increment");
        let dec_button = gtk::Button::with_label("Decrement");

        let label = gtk::Label::new(Some(&format!("Counter: {}", model.counter)));
        label.set_margin_all(5);

        window.set_child(Some(&vbox));
        vbox.set_margin_all(5);
        vbox.append(&inc_button);
        vbox.append(&dec_button);
        vbox.append(&label);

        inc_button.connect_clicked(clone!(@strong sender => move |_| {
            sender.input(AppInput::Increment);
        }));

        dec_button.connect_clicked(clone!(@strong sender => move |_| {
            sender.input(AppInput::Decrement);
        }));

        let widgets = AppWidgets { label };

        ComponentParts { model, widgets }
    }

    fn update(&mut self, message: Self::Input, _sender: ComponentSender<Self>) {
        match message {
            AppInput::Increment => {
                self.counter = self.counter.wrapping_add(1);
            }
            AppInput::Decrement => {
                self.counter = self.counter.wrapping_sub(1);
            }
        }
    }

    /// Update the view to represent the updated model.
    fn update_view(&self, widgets: &mut Self::Widgets, _sender: ComponentSender<Self>) {
        widgets
            .label
            .set_label(&format!("Counter: {}", self.counter));
    }
}

fn main() {
    let app = RelmApp::new("relm4.test.simple_manual");
    app.run::<AppModel>(0);
}
```

<https://relm4.org/book/stable/first_app.html>

# Relm4 more example

<https://github.com/Relm4/Relm4/tree/main/examples>

# Summary

Hope I have convinced you that Declarative UIs are simpler and easier
to maintain, debug, and scale.

- Go through <https://guide.elm-lang.org>
- Try <https://owickstrom.github.io/gi-gtk-declarative/>
- Play with <https://relm4.org/book/stable/>

Many other interesting reactive/declarative projects in development,
but in this tutorial talk I tried to focus on the purest kinds.

Blog post: <https://engineering.rakuten.today/post/elm-at-rakuten/>

Checkout also:

- <https://github.com/reflex-frp>
- <https://github.com/fjvallarino/monomer>
- <https://github.com/utkarshkukreti/purescript-hedwig>
- <https://can-lehmann.github.io/owlkettle/README.html>
- Functional Reactive Programming (FRP)

Like Haskell, even if you don't continue Declarative UI programming,
learning it will still make you a better application programmer.
