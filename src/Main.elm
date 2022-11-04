port module Main exposing (main)

import Browser
import Css exposing (..)
import Date exposing (Date, Interval(..), Unit(..), today)
import Html exposing (..)
import Html.Attributes exposing (class, name, src, style)
import Html.Events exposing (onClick)
import ListView exposing (..)
import Task exposing (..)
import Time exposing (Month(..), utc)


type alias Developer =
    { name : String
    , date : Date
    }


type alias Model =
    { tableState : ListView.State
    , dateJs : Date
    , developers : List Developer
    , dateJsString : String
    }


defaultDate =
    Date.fromPosix utc (Time.millisToPosix 0)


init : ( Model, Cmd Msg )
init =
    ( { tableState = ListView.makeState
      , dateJs = Date.fromPosix utc (Time.millisToPosix 0)
      , developers = [ Developer "Bartek" defaultDate, Developer "Lybron" defaultDate, Developer "River" defaultDate, Developer "Mika" defaultDate ]
      , dateJsString = ""
      }
    , sendData "Hello Js"
    )


type Msg
    = ReceivedDataFromJS String
    | SendDataToJS
    | ModifyList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendDataToJS ->
            ( model, sendData "Hello Js" )

        ReceivedDataFromJS data ->
            ( { model | dateJs = format (String.slice 0 10 data), dateJsString = data }, Cmd.none )

        ModifyList ->
            ( { model | developers = newHead model }, Cmd.none )


port receiveData : (String -> msg) -> Sub msg


port sendData : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveData ReceivedDataFromJS


view : Model -> Html Msg
view model =
    Html.div []
        [ h1 [ class "title" ] [ text "Click on the Unicorn to assign a new one" ]
        , Html.div
            [ class "container" ]
            [ button [ onClick ModifyList, style "border-style" "none", style "padding" "0" ] [ img [ src "src/unicorn-bg.png", style "width" "200px", style "height" "200px", style "background-color" "pink", style "padding" "0" ] [] ]
            ]
        , div [ class "container" ] [ div [] (renderProducts (List.sortBy (\x -> Date.toIsoString x.date) model.developers)) ]
        ]


renderProduct : Developer -> Html msg
renderProduct product =
    let
        children =
            [ li [ style "list-style" "none" ] [ text product.name ]
            , li [ style "list-style" "none" ] [ text (Date.toIsoString product.date) ]
            ]
    in
    ul [ style "width" "200px" ] children


renderProducts : List Developer -> List (Html msg)
renderProducts products =
    List.map renderProduct products


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


newHead : Model -> List Developer
newHead model =
    let
        sortedList =
            List.sortBy (\x -> Date.toIsoString x.date) model.developers

        updateUser user =
            if user.date == model.dateJs then
                user

            else
                { user | date = model.dateJs }

        headList =
            List.take 1 sortedList |> List.map updateUser

        newList =
            List.drop 1 sortedList
    in
    List.append newList headList


format : String -> Date
format x =
    case x of
        _ ->
            Date.fromIsoString x |> Result.withDefault (Date.fromPosix utc (Time.millisToPosix 0))
