module Main exposing (main)

import Browser
import Dict
import Html exposing (Html, button, div, input, text)
import Html.Attributes as Attrs
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



-- LENSES


inputValueL : Lens Model String
inputValueL =
    Lens .inputValue (\b a -> { a | inputValue = b })


stateIdL : Lens Model Stuff.Id
stateIdL =
    Lens .idState (\b a -> { a | idState = b })


todosL : Lens Model (List Todo)
todosL =
    Lens .todos (\b a -> { a | todos = b })


filterL : Lens Model Filter
filterL =
    Lens .filter (\b a -> { a | filter = b })



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case Debug.log "msg" msg of
        InputChange value ->
            inputValueL.set value model

        KeyPress keyCode ->
            if keyCode == 13 && not (String.isEmpty (inputValueL.get model)) then
                let
                    newTodoId =
                        Stuff.nextId (stateIdL.get model)

                    newTodo =
                        Todo.createTodo (inputValueL.get model) False newTodoId

                    nextTodos =
                        model.todos ++ [ newTodo ]
                in
                model
                    |> todosL.set nextTodos
                    |> inputValueL.set ""
                    |> stateIdL.set newTodoId

            else
                model

        DeleteTodo id ->
            let
                isGood t =
                    Stuff.flatId t.id /= Stuff.flatId id

                nextTodos =
                    List.filter isGood (todosL.get model)
            in
            model
                |> todosL.set nextTodos

        ToggleTodo cid checked ->
            let
                map t =
                    if t.id == cid then
                        Todo.createTodo t.text checked t.id

                    else
                        t

                nextTodos =
                    List.map map model.todos
            in
            model
                |> todosL.set nextTodos

        ToggleAllTodos ->
            let
                allChecked =
                    List.all (\t -> t.checked)

                mapTodo chkd t =
                    Todo.createTodo t.text chkd t.id

                nextTodos =
                    if allChecked model.todos then
                        List.map (mapTodo False) model.todos

                    else
                        List.map (mapTodo True) model.todos
            in
            model
                |> todosL.set nextTodos

        SetFilter filter ->
            model
                |> filterL.set filter

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


view : Model -> Html Msg
view m =
    let
        todosFilter : Filter -> List Todo -> List Todo
        todosFilter filter todos =
            let
                predicate todo =
                    case filter of
                        All ->
                            True

                        Done ->
                            todo.checked

                        Remaining ->
                            not todo.checked
            in
            List.filter predicate todos

        --
    in
    div []
        [ div [] (List.map todoElement << todosFilter m.filter <| m.todos)
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
