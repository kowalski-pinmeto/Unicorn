port module Main exposing (main)

import Array
import Browser
import Css exposing (..)
import Date exposing (Date, Interval(..), Unit(..), today)
import Html exposing (..)
import Html.Attributes exposing (name)
import Html.Events exposing (onClick)
import ListView exposing (..)
import ListView.Viewers
import Task exposing (..)
import Time exposing (..)


type alias Developer =
    { name : String
    , date : String
    }


type alias Model =
    { tableState : ListView.State
    , dateJs : String
    , developers : List Developer
    }


init : ( Model, Cmd Msg )
init =
    ( { tableState = ListView.makeState
      , dateJs = ""
      , developers = [ Developer "A" "2", Developer "A" "1", Developer "A" "5" ]
      }
    , sendData "Hello Js"
    )


type SortDirection
    = ASC
    | DESC


type ColumnSortInfo
    = UnsortableColumn
    | UnsortedColumn
    | SortedColumn SortDirection


tableConfig : Config Developer Msg
tableConfig =
    ListView.makeConfig
        |> ListView.withColumn (ListView.makeColumn.string "Name" .name)
        |> ListView.withColumn (ListView.makeColumn.string "Date" .date)


type Msg
    = OnTableMsg ListView.Viewers.ListViewMsg
    | ReceivedDataFromJS String
    | SendDataToJS
    | UpdateTable String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTableMsg tableMsg ->
            ( { model | tableState = ListView.Viewers.update model.developers tableMsg model.tableState }, Cmd.none )

        SendDataToJS ->
            ( model, sendData "Hello Js" )

        ReceivedDataFromJS data ->
            ( { model | dateJs = data }, appendList )


port receiveData : (String -> msg) -> Sub msg


port sendData : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveData ReceivedDataFromJS


appendList : Model -> List Developer
appendList model =
    List.append model.developers [ Developer "A" "B" ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ ListView.Viewers.viewAsHtmlTable OnTableMsg tableConfig model.tableState model.developers
            ]
        , Html.div
            []
            [ Html.text model.dateJs ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
