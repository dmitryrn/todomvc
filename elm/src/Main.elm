module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Json
import Stuff
import Todo exposing (Todo)



-- MAIN


main : Program () Model Msg
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


type Filter
    = All
    | Done
    | Remaining


type alias Model =
    { todos : List Todo
    , inputValue : String
    , idState : Stuff.Id
    , filter : Filter
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { todos = []
      , inputValue = ""
      , idState = Stuff.createId 0
      , filter = All
      }
    , Cmd.none
    )


type Msg
    = InputChange String
    | KeyPress Int
    | DeleteTodo Stuff.Id
    | ToggleTodo Stuff.Id Bool
    | ToggleAllTodos
    | SetFilter Filter
    | NoOp



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        InputChange value ->
            { model | inputValue = value }

        KeyPress keyCode ->
            if keyCode == 13 && not (String.isEmpty model.inputValue) then
                let
                    newTodoId =
                        Stuff.nextId model.idState

                    newTodo =
                        Todo.createTodo model.inputValue False newTodoId

                    nextTodos =
                        model.todos ++ [ newTodo ]
                in
                { model
                    | inputValue = ""
                    , todos = nextTodos
                    , idState = newTodoId
                }

            else
                model

        DeleteTodo id ->
            let
                isGood t =
                    Stuff.flatId t.id /= Stuff.flatId id

                nextTodos =
                    List.filter isGood model.todos
            in
            { model | todos = nextTodos }

        ToggleTodo toggleId checked ->
            let
                mapTodo todo =
                    if todo.id == toggleId then
                        Todo.createTodo todo.text checked todo.id

                    else
                        todo

                nextTodos =
                    List.map mapTodo model.todos
            in
            { model | todos = nextTodos }

        ToggleAllTodos ->
            let
                mapTodo checked todo =
                    Todo.createTodo todo.text checked todo.id

                nextTodos =
                    if List.all .checked model.todos then
                        List.map (mapTodo False) model.todos

                    else
                        List.map (mapTodo True) model.todos
            in
            { model | todos = nextTodos }

        SetFilter filter ->
            { model | filter = filter }

        NoOp ->
            model
    , Cmd.none
    )



-- VIEW


todoElement : Todo -> Html Msg
todoElement t =
    div []
        [ div [] [ text t.text ]
        , button
            [ Events.onClick (DeleteTodo t.id)
            ]
            [ text "x" ]
        , input
            [ Attrs.type_ "checkbox"
            , Attrs.checked t.checked
            , Events.onCheck (ToggleTodo t.id)
            ]
            []
        ]


filterElement : Filter -> Html Msg
filterElement current =
    let
        onClick filter =
            Events.onClick (SetFilter filter)

        map ( str, t ) =
            if t == current then
                button
                    [ Attrs.style "border" "1px solid"
                    , onClick t
                    ]
                    [ text str ]

            else
                button [ onClick t ] [ text str ]

        values =
            [ ( "all", All ), ( "done", Done ), ( "remaining", Remaining ) ]
    in
    div [] (List.map map values)


filterPredicate : Filter -> (Todo -> Bool)
filterPredicate filter todo =
    case filter of
        All ->
            True

        Done ->
            todo.checked

        Remaining ->
            not todo.checked


view : Model -> Html Msg
view m =
    div []
        [ div [] (List.map todoElement << List.filter (filterPredicate m.filter) <| m.todos)
        , div []
            [ input
                [ Attrs.value m.inputValue
                , Events.onInput InputChange
                , Events.on "keypress" (Json.map KeyPress Events.keyCode)
                ]
                []
            , button
                [ Events.onClick ToggleAllTodos ]
                [ text "toggle checked" ]
            ]
        , filterElement m.filter
        , div [] [ text ((String.fromInt << List.length << List.filter (\t -> not t.checked) <| m.todos) ++ " remaining") ]
        ]
