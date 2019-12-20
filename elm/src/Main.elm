module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, value)
import Html.Events as Events
import Json.Decode as Json
import Monocle.Lens exposing (Lens)



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


type alias Todo =
    { text : String
    , id : Id
    }


type alias Model =
    { todos : List Todo
    , inputValue : String
    , internalId : Id
    }


type Id
    = Id Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { todos = []
      , inputValue = ""
      , internalId = Id 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputChange String
    | KeyPress Int
    | NoOp


inputValueOfModel : Lens Model String
inputValueOfModel =
    Lens .inputValue (\b a -> { a | inputValue = b })


internalIdOfModel : Lens Model Id
internalIdOfModel =
    Lens .internalId (\b a -> { a | internalId = b })


todosOfModel : Lens Model (List Todo)
todosOfModel =
    Lens .todos (\b a -> { a | todos = b })


flatId : Id -> Int
flatId id =
    case id of
        Id n ->
            n


mapId : (Int -> Int) -> Id -> Id
mapId f id =
    Id << f << flatId <| id


nextInternalid : Id -> Id
nextInternalid id =
    mapId (\x -> x + 1) id


createTodo : String -> Id -> Todo
createTodo text id =
    { text = text
    , id = id
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case Debug.log "msg" msg of
        InputChange value ->
            inputValueOfModel.set value model

        KeyPress keyCode ->
            if keyCode == 13 && not (String.isEmpty (inputValueOfModel.get model)) then
                let
                    newTodoId =
                        nextInternalid (internalIdOfModel.get model)

                    newTodo =
                        createTodo (inputValueOfModel.get model) newTodoId

                    nextTodos =
                        model.todos ++ [ newTodo ]
                in
                model
                    |> todosOfModel.set nextTodos
                    |> inputValueOfModel.set ""
                    |> internalIdOfModel.set newTodoId

            else
                model

        NoOp ->
            model
    , Cmd.none
    )



-- VIEW


singleTodo : Todo -> Html Msg
singleTodo todo =
    div [ class "todo-item" ] [ text <| String.concat [ todo.text, " ", String.fromInt <| flatId todo.id ] ]


todosList : List Todo -> List (Html Msg)
todosList todos =
    List.map singleTodo todos


view : Model -> Html Msg
view model =
    div []
        [ div [] (todosList model.todos)
        , input
            [ value model.inputValue
            , Events.onInput InputChange
            , Events.on "keypress" (Json.map KeyPress Events.keyCode)
            ]
            []
        ]
