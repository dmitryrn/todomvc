module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (..)
import Monocle.Lens exposing (Lens)
import Json.Decode as Json


-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



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

type Id = Id Int

init : Model
init =
    { todos = []
    , inputValue = ""
    , internalId = Id 0
    }



-- UPDATE


type Msg
    = InputChange String
    | KeyPress Int


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
        Id n -> n

mapId : (Int -> Int) -> Id -> Id
mapId f id = Id << f << flatId <| id

nextInternalid : Id -> Id
nextInternalid id = mapId (\x -> x + 1) id

createTodo : String -> Id -> Todo
createTodo text id =
    { text = text
    , id = id
    }

update : Msg -> Model -> Model
update msg model =
    case Debug.log "msg" msg of
        InputChange value ->
            inputValueOfModel.set value model

        KeyPress keyCode ->
            if
                keyCode == 13 && not (String.isEmpty (inputValueOfModel.get model))
            then
                let
                    newTodoId = nextInternalid (internalIdOfModel.get model)
                    newTodo = createTodo (inputValueOfModel.get model) newTodoId
                    nextTodos =
                        model.todos ++ [ newTodo ]
                in
                    model
                        |> todosOfModel.set nextTodos
                        |> inputValueOfModel.set ""
                        |> internalIdOfModel.set newTodoId
            else model



-- VIEW


singleTodo : Todo -> Html Msg
singleTodo todo =
    div [ class "todo-item" ] [ text <| String.concat [todo.text, " ", (String.fromInt <| flatId todo.id)] ]


todosList : List Todo -> List (Html Msg)
todosList todos =
    List.map singleTodo todos


view : Model -> Html Msg
view model =
    div []
        [ div [] (todosList model.todos)
        , input
            [ value model.inputValue
            , onInput InputChange
            , on "keypress" (Json.map KeyPress keyCode)
            ]
            []
        ]
