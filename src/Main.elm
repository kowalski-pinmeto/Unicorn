port module Main exposing (main)

import Browser
import Css exposing (..)
import Date exposing (Date, Interval(..), Unit(..), today)
import Html exposing (..)
import Html.Attributes exposing (class, id, name, src, style, type_)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE
import List.Extra
import ListView exposing (..)
import Svg.Styled.Attributes exposing (x)
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
    , json : String
    , jsonJS : String
    }


defaultDate =
    Date.fromPosix utc (Time.millisToPosix 0)


formatDate date =
    Date.fromPosix utc (Time.millisToPosix date)


init : ( Model, Cmd Msg )
init =
    ( { tableState = ListView.makeState
      , dateJs = Date.fromPosix utc (Time.millisToPosix 0)
      , developers = []
      , dateJsString = ""
      , newDev = ""
      , collapsible = True
      , sickDevelopers = []
      , json = ""
      , jsonJS = ""
      }
    , sendData "Date pls"
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
            ( { model | dateJs = format (String.slice 0 10 data), dateJsString = String.slice 0 10 data, jsonJS = String.dropLeft 39 data |> String.dropRight 3, developers = oldDevs data False, sickDevelopers = oldDevs data True }, Cmd.none )

        ModifyList ->
            ( { model | developers = newHead model, json = createJson (newHead model) model.sickDevelopers }, sendData (createJson (newHead model) model.sickDevelopers) )

        Input text ->
            ( { model | newDev = text }, Cmd.none )

        UpdateDevs ->
            let
                dev =
                    Developer model.newDev defaultDate None
            in
            if String.length model.newDev > 1 then
                ( { model | collapsible = True, developers = dev :: model.developers, json = createJson (dev :: model.developers) model.sickDevelopers }, sendData (createJson (dev :: model.developers) model.sickDevelopers) )

            else
                ( { model | collapsible = True }, Cmd.none )

        OpenButton ->
            ( { model | collapsible = False }, Cmd.none )

        Nope ->
            ( model, Cmd.none )

        SickStatus status name ->
            if status == True then
                ( { model | developers = markAsSick name model.developers status, sickDevelopers = pushToSick model name status, json = createJson (markAsSick name model.developers status) (pushToSick model name status) }, sendData (createJson (markAsSick name model.developers status) (pushToSick model name status)) )

            else
                ( { model | developers = backToHealth name model, sickDevelopers = removeHealthy model.sickDevelopers name, json = createJson (backToHealth name model) (removeHealthy model.sickDevelopers name) }, sendData (createJson (backToHealth name model) (removeHealthy model.sickDevelopers name)) )

        AssignUnicorn name ->
            ( { model | developers = assignUnicorn model name, json = createJson (assignUnicorn model name) model.sickDevelopers }, sendData (createJson (assignUnicorn model name) model.sickDevelopers) )


port receiveData : (String -> msg) -> Sub msg


port sendData : String -> Cmd msg


oldDevs : String -> Bool -> List Developer
oldDevs data isSick =
    let
        dropStuff x =
            String.dropLeft 36 x |> String.dropRight 2 |> splitOne |> checkIfSick

        checkIfSick x =
            if isSick == False then
                List.map removeSicks x

            else
                List.map keepSicks x

        removeSicks x =
            if String.contains "Sick" x then
                ""

            else
                x

        keepSicks x =
            if String.contains "Sick" x then
                x

            else
                ""

        splitOne =
            String.split "&"

        splitTwo x =
            List.map (String.split ";") x

        transformToDev x =
            Developer (Maybe.withDefault "name" (List.Extra.getAt 0 x)) (Result.withDefault (formatDate 1666952162000) (Date.fromIsoString (Maybe.withDefault "2022-01-01" (List.Extra.getAt 1 x)))) (getStatus (Maybe.withDefault "None" (List.Extra.getAt 2 x)))

        getStatus x =
            case x of
                "Unicorn" ->
                    Unicorn

                "None" ->
                    None

                _ ->
                    Sick
    in
    dropStuff data |> splitTwo |> List.map transformToDev


createJson : List Developer -> List Developer -> String
createJson devs sickos =
    let
        statusString x =
            case x of
                Unicorn ->
                    "Unicorn"

                Sick ->
                    "Sick"

                _ ->
                    "None"

        dateToString x =
            Date.toIsoString x

        devToString x =
            x.name ++ ";" ++ dateToString x.date ++ ";" ++ statusString x.status

        stringList =
            List.map devToString connectLists

        connectLists =
            List.append devs sickos
    in
    String.join "&" stringList


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveData ReceivedDataFromJS


view : Model -> Html Msg
view model =
    div [ class "griddy" ]
        [ h1 [ class "title" ] [ text "Click on the Unicorn to assign a new one" ]
        , Html.div
            [ class "container" ]
            [ button [ onClick ModifyList, style "border-style" "none", style "padding" "0" ] [ img [ src "src/unicorn-bg.png", style "width" "200px", style "height" "200px", style "background-color" "pink", style "padding" "0" ] [] ]
            ]
        , if List.length model.sickDevelopers > 0 then
            section [ class "sickos" ]
                [ h3 [] [ text "Sick unicorns :(" ]
                , div [] (renderSicks model.sickDevelopers)
                ]

          else
            text ""
        , div [ class "devs-list" ] (renderProducts (List.sortBy (\x -> Date.toIsoString x.date) model.developers))
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
        ul [ class unicornRender ]
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
                , button [ onClick (SickStatus False dev.name) ] [ img [ src "src/healthy.png", style "width" "20px" ] [] ]
                ]

            else
                []
    in
    ul [ style "display" "flex" ] kid


pushToSick : Model -> String -> Bool -> List Developer
pushToSick model name status =
    let
        devs =
            model.developers

        sicks =
            model.sickDevelopers

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


backToHealth : String -> Model -> List Developer
backToHealth name model =
    let
        check x =
            if x.name == name then
                True

            else
                False

        isName x =
            x.name == name

        sicks =
            model.sickDevelopers

        devs =
            model.developers

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
