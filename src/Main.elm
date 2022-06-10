module Main exposing (..)

import Browser
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.CDN exposing (..)
import Html exposing (Html, main_, text, div, h1, h2, img, input)
import Html.Attributes exposing (src)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import Array

---- MODEL ----


type alias Model =
    { content : String
    , tasks: List Task
    }

type State
    = Incomplete
    | Completed

type alias Task =
    { id : Int
    , name : String
    , state: State
    }

init : ( Model, Cmd Msg )
init =
    ( { content = "", tasks = [{id = 1, name = "Add more items to the list", state = Incomplete}] }, Cmd.none )



---- UPDATE & DELETE ----


type Msg
    = Change String
    | Add String
    | Delete Int
    | Update Task

getNewId model = 
    Array.length (Array.fromList model.tasks) + 1

getNewTaskList model newContent = 
    model.tasks ++ [{id = getNewId model, name = newContent, state = Incomplete}]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            ({ model | content = newContent }, Cmd.none)
        Add newContent ->
            ({ model | content = model.content, tasks = getNewTaskList model newContent }, Cmd.none)
        Delete id ->
            ({ model | content = model.content, tasks = 
                List.filter (\x -> x.id /= id) model.tasks 
                    |> List.indexedMap (\i x -> {id = (x.id * 0) + (i + 1), name = x.name, state = x.state})}, Cmd.none)
        Update currentTask ->
            ({ model | content = model.content, tasks = 
                (Array.fromList model.tasks
                    |> Array.set (currentTask.id - 1) {id = currentTask.id, name = currentTask.name, state = Completed} 
                    |> Array.toList
                )}, Cmd.none)

tasksTable model = 
    table
        { bordered = True
        , striped = True
        , narrow = True
        , hoverable = True
        , fullWidth = True
        }
        []
        [ tableHead []
            [ tableRow False
                []
                [ tableCellHead [] [ text "ID" ]
                , tableCellHead [] [ text "Name" ]
                , tableCellHead [] [ text "Status" ]
                ]
            ]
        , tableBody [] (List.map formatRow model.tasks)
        , tableFoot [] []
        ]

formatRow : Task -> Html Msg
formatRow task =
    tableRow False
        []
        [ tableCell [] [ text (String.fromInt task.id) ]
        , tableCell [] [ text task.name ]
        , tableCell [] [ formatState task ]
        ]

formatState : Task -> Html Msg
formatState task =
    case task.state of
        Completed ->
            div []
                [button { buttonModifiers | color = Primary }
                    [ onClick (Delete task.id)]
                    [ text "Completed! Click to Remove" ]
                ]
            
        Incomplete ->
            div []
                [button { buttonModifiers | color = Warning }
                    [ onClick (Update task)]
                    [ text "Incomplete" ]
                , button { buttonModifiers | color = Primary }
                    [ onClick (Delete task.id) ]
                    [ text "Remove" ]
                ]
            

---- VIEW ----


view : Model -> Html Msg
view model =
    main_ []
            [ stylesheet
            ,hero
                { bold = False
                , size = Standard
                , color = Default
                }
                []
                [ heroBody []
                    [ container []
                        [ title H1 [] [ text "My tasks" ]
                        , 
                            tasksTable model
                        ]
                    , input [placeholder "Type new todo here", onInput Change] []
                    , button { buttonModifiers | color = Primary }
                        [ onClick (Add model.content) ]
                        [ text "Add Task" ]
                    ]
                ]
            ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
