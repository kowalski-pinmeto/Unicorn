port module Main exposing (main)

import Browser
import Css exposing (..)
import Date exposing (Date, Interval(..), Unit(..), today)
import Html exposing (..)
import Html.Attributes exposing (class, name, src, style)
import Html.Events exposing (onClick, onInput)
import ListView exposing (..)
import Task exposing (..)
import Time exposing (Month(..), utc)


type Status
    = Unicorn
    | Sick
    | None


type alias Developer =
    { name : String
    , date : Date
    , status : Status
    }


type alias Model =
    { tableState : ListView.State
    , dateJs : Date
    , developers : List Developer
    , dateJsString : String
    , newDev : String
    , collapsible : Bool
    , sickDevelopers : List Developer
    }


defaultDate =
    Date.fromPosix utc (Time.millisToPosix 0)


formatDate date =
    Date.fromPosix utc (Time.millisToPosix date)


init : ( Model, Cmd Msg )
init =
    ( { tableState = ListView.makeState
      , dateJs = Date.fromPosix utc (Time.millisToPosix 0)
      , developers = [ Developer "Bartek" (formatDate 1666952162000) None, Developer "Lybron" (formatDate 1667470562000) None, Developer "River" (formatDate 1667384162000) None, Developer "Mika" (formatDate 1667297762000) None ]
      , dateJsString = ""
      , newDev = ""
      , collapsible = True
      , sickDevelopers = []
      }
    , sendData "Hello Js"
    )


type Msg
    = ReceivedDataFromJS String
    | SendDataToJS
    | ModifyList
    | Input String
    | UpdateDevs
    | OpenButton
    | Nope
    | SickStatus Bool String
    | AssignUnicorn String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendDataToJS ->
            ( model, sendData "Hello Js" )

        ReceivedDataFromJS data ->
            ( { model | dateJs = format (String.slice 0 10 data), dateJsString = data }, Cmd.none )

        ModifyList ->
            ( { model | developers = newHead model }, Cmd.none )

        Input text ->
            ( { model | newDev = text }, Cmd.none )

        UpdateDevs ->
            let
                dev =
                    Developer model.newDev defaultDate None
            in
            if String.length model.newDev > 1 then
                ( { model | collapsible = True, developers = dev :: model.developers }, Cmd.none )

            else
                ( { model | collapsible = True }, Cmd.none )

        OpenButton ->
            ( { model | collapsible = False }, Cmd.none )

        Nope ->
            ( model, Cmd.none )

        SickStatus status name ->
            if status == True then
                ( { model | developers = markAsSick name model.developers status, sickDevelopers = pushToSick model.developers model.sickDevelopers name status }, Cmd.none )

            else
                ( { model | developers = backToHealth name model.developers model.sickDevelopers, sickDevelopers = removeHealthy model.sickDevelopers name }, Cmd.none )

        AssignUnicorn name ->
            ( { model | developers = assignUnicorn model name }, Cmd.none )


port receiveData : (String -> msg) -> Sub msg


port sendData : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveData ReceivedDataFromJS


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "title" ] [ text "Click on the Unicorn to assign a new one" ]
        , Html.div
            [ class "container" ]
            [ button [ onClick ModifyList, style "border-style" "none", style "padding" "0" ] [ img [ src "src/unicorn-bg.png", style "width" "200px", style "height" "200px", style "background-color" "pink", style "padding" "0" ] [] ]
            ]
        , div [ class "container-bottom" ]
            [ if List.length model.sickDevelopers > 0 then
                section [ class "sickos" ]
                    [ h3 [] [ text "Sick unicorns :(" ]
                    , div [ class "sickos" ] (renderSicks model.sickDevelopers)
                    ]

              else
                text ""
            , div [ class "devs-list" ] (renderProducts (List.sortBy (\x -> Date.toIsoString x.date) model.developers))
            , img [ src "src/collapsible.png", class "wow" ] []
            , button
                [ class "myButton"
                , if model.collapsible == True then
                    onClick OpenButton

                  else
                    onClick Nope
                ]
                [ text "Add Unicorn"
                , if model.collapsible == True then
                    div [] []

                  else
                    div
                        []
                        [ input [ class "container", onInput Input ] []
                        , button [ onClick UpdateDevs ] [ text "Submit" ]
                        ]
                ]
            ]
        ]


renderProduct : Developer -> Html Msg
renderProduct product =
    let
        unicornRender =
            if product.status == Unicorn then
                "unicorn"

            else
                "dev-ul"
    in
    if product.status /= Sick then
        ul [ class unicornRender, style "width" "30%" ]
            [ div []
                [ li [ class "dev-li" ]
                    [ text product.name ]
                , li [ class "dev-li", style "font-size" "90%" ]
                    [ if product.date == formatDate 1603880162000 then
                        text "Next unicorn"

                      else
                        text (Date.toIsoString product.date)
                    ]
                ]
            , section
                [ style "text-align-last" "center" ]
                [ if product.status == Unicorn then
                    button [ onClick (SickStatus True product.name), class "sickUnicorn", style "width" "100px", style "height" "100px" ] []

                  else
                    button [ class "buttons", onClick (AssignUnicorn product.name) ] [ text "Assign" ]
                ]
            ]

    else
        text ""


renderProducts : List Developer -> List (Html Msg)
renderProducts products =
    List.reverse products |> List.map renderProduct


renderSicks : List Developer -> List (Html Msg)
renderSicks sicks =
    let
        makeSick x =
            { x | status = Sick }
    in
    List.map makeSick sicks |> List.map renderSick


statusToString : Status -> Html Msg
statusToString status =
    case status of
        Unicorn ->
            text "Unicorn"

        Sick ->
            text "Sick"

        _ ->
            text ""


assignUnicorn : Model -> String -> List Developer
assignUnicorn model name =
    let
        noUnicorns x =
            if x.status == Sick then
                x

            else
                { x | status = None }

        assign x =
            if x.name == name then
                { x | status = Unicorn, date = model.dateJs }

            else
                x
    in
    List.map noUnicorns model.developers |> List.map assign


renderSick : Developer -> Html Msg
renderSick dev =
    let
        kid =
            if dev.status == Sick then
                [ li [ style "list-style" "none" ] [ text dev.name ]
                , button [ onClick (SickStatus False dev.name), style "margin-left" "5px" ] [ img [ src "src/healthy.png", style "width" "20px" ] [] ]
                ]

            else
                []
    in
    ul [ style "display" "flex" ] kid


pushToSick : List Developer -> List Developer -> String -> Bool -> List Developer
pushToSick devs sicks name status =
    let
        isSick dev =
            dev.name == name
    in
    List.filter isSick devs |> List.append sicks


markAsSick : String -> List Developer -> Bool -> List Developer
markAsSick sickName devs status =
    let
        sickCheck x =
            if x.name == sickName then
                if status == True then
                    { x | status = Sick, date = formatDate 1603880162000 }

                else
                    { x | status = None }

            else
                x
    in
    if status == True then
        List.map sickCheck devs

    else
        devs


removeHealthy : List Developer -> String -> List Developer
removeHealthy sicks name =
    let
        isName x =
            x.name /= name
    in
    List.filter isName sicks


backToHealth : String -> List Developer -> List Developer -> List Developer
backToHealth name devs sicks =
    let
        check x =
            if x.name == name then
                True

            else
                False

        isName x =
            x.name == name

        healthyGuy =
            List.filter isName sicks

        makeBetter x =
            if x.name == name then
                { x | status = None, date = formatDate 1603880162000 }

            else
                x
    in
    if List.member True (List.map check devs) then
        List.map makeBetter devs

    else
        List.append healthyGuy devs |> List.map makeBetter


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
        isSick dev =
            dev.status /= Sick

        sortedList =
            List.filter isSick model.developers |> List.sortBy (\x -> Date.toIsoString x.date)

        updateUser user =
            { user | date = model.dateJs, status = Unicorn }

        noUnicorns x =
            { x | status = None }

        headList =
            List.map noUnicorns sortedList |> List.take 1 |> List.map updateUser

        newList =
            List.map noUnicorns sortedList |> List.drop 1
    in
    List.append newList headList |> List.filter isSick


format : String -> Date
format x =
    case x of
        _ ->
            Date.fromIsoString x |> Result.withDefault (Date.fromPosix utc (Time.millisToPosix 0))
