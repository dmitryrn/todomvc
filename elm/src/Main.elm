module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, value)
import Html.Events as Events
import Json.Decode as Json
import Monocle.Lens exposing (Lens)
import Stuff
import Todo exposing (Todo)



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Model =
    { todos : List Todo
    , inputValue : String
    , idState : Stuff.Id
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { todos = []
      , inputValue = ""
      , idState = Stuff.createId 0
      }
    , Cmd.none
    )


type Msg
    = InputChange String
    | KeyPress Int
    | NoOp



-- LENSES


inputValueOfModel : Lens Model String
inputValueOfModel =
    Lens .inputValue (\b a -> { a | inputValue = b })


stateIdOfModel : Lens Model Stuff.Id
stateIdOfModel =
    Lens .idState (\b a -> { a | idState = b })


todosOfModel : Lens Model (List Todo)
todosOfModel =
    Lens .todos (\b a -> { a | todos = b })



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case Debug.log "msg" msg of
        InputChange value ->
            inputValueOfModel.set value model

        KeyPress keyCode ->
            if keyCode == 13 && not (String.isEmpty (inputValueOfModel.get model)) then
                let
                    newTodoId =
                        Stuff.nextId (stateIdOfModel.get model)

                    newTodo =
                        Todo.createTodo (inputValueOfModel.get model) newTodoId

                    nextTodos =
                        model.todos ++ [ newTodo ]
                in
                model
                    |> todosOfModel.set nextTodos
                    |> inputValueOfModel.set ""
                    |> stateIdOfModel.set newTodoId

            else
                model

        NoOp ->
            model
    , Cmd.none
    )



-- VIEW


todo : Todo -> Html msg
todo t =
    div [] [ text t.text ]


view : Model -> Html Msg
view m =
    div []
        [ div [] (List.map todo m.todos)
        , Html.input
            [ value m.inputValue
            , Events.onInput InputChange
            , Events.on "keypress" (Json.map KeyPress Events.keyCode)
            ]
            []
        ]
