module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, br, button, div, li, p, text, ul)
import Html.Attributes as Att exposing (attribute, disabled)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Railroad as RR
import Railroad.Layout as Layout
import Round
import Svg exposing (Svg, circle, g, line, svg)
import Svg.Attributes exposing (..)
import Time exposing (posixToMillis)


main =
    Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { initialState : Maybe RR.State
    , state : Maybe RR.State
    , scale : Float
    , millis : Maybe Int
    , frame : Int
    , isRunning : Bool
    }


type Msg
    = Tick Time.Posix
    | Start
    | Stop
    | Reset


init : () -> ( Model, Cmd Msg )
init _ =
    let
        s =
            RR.sample
    in
    ( { initialState = s
      , state = s
      , scale = 1
      , millis = Nothing
      , frame = 0
      , isRunning = False
      }
    , Cmd.none
    )


subscriptions _ =
    Time.every 100 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                newMillis =
                    posixToMillis newTime

                duration =
                    case model.millis of
                        Nothing ->
                            0

                        Just m ->
                            newMillis - m
            in
            ( { model
                | millis = Just newMillis
                , state =
                    case model.state of
                        Just s ->
                            if model.isRunning then
                                Just (RR.moved duration s)

                            else
                                model.state

                        Nothing ->
                            Nothing
                , frame = duration
              }
            , Cmd.none
            )

        Start ->
            ( { model | isRunning = True }, Cmd.none )

        Stop ->
            ( { model | isRunning = False }, Cmd.none )

        Reset ->
            ( { model | state = model.initialState }, Cmd.none )


type alias Document msg =
    { title : String, body : List (Html msg) }


view : Model -> Document Msg
view model =
    { title = "Railroad"
    , body =
        [ div [ class "container" ]
            [ div [ class "row mt-3" ]
                [ div [ class "col" ]
                    [ svg
                        [ width "100%"
                        , height "100%"
                        , Att.style "border" "1px solid black"
                        , model.scale |> scaleTransform |> transform
                        ]
                        (case model.state of
                            Just state ->
                                [ g [] (List.map trackToSvg (Layout.tracks state.layout))
                                , g [] (List.map connectorToSvg (Layout.connectors state.layout))
                                , g [] (List.map trainToSvg state.trains)
                                ]

                            Nothing ->
                                []
                        )
                    ]
                ]
            , div [ class "row mt-3" ]
                [ div [ class "col" ]
                    [ lazy viewSimulationControls model.isRunning ]
                ]
            ]
        ]
    }


viewSimulationControls : Bool -> Html Msg
viewSimulationControls isRunning =
    div []
        [ if isRunning then
            button [ class "btn btn-primary", onClick Stop ] [ text "Stop simulation" ]

          else
            button [ class "btn btn-primary", onClick Start ] [ text "Start simulation" ]
        , button
            [ class "btn btn-secondary ml-2"
            , if isRunning then
                disabled True

              else
                onClick Reset
            ]
            [ text "Reset" ]
        ]


scaleTransform : Float -> String
scaleTransform scale =
    let
        scaleStr =
            String.fromFloat scale
    in
    "scale(" ++ scaleStr ++ " " ++ scaleStr ++ ")"


trackToSvg : Layout.Track -> Svg Msg
trackToSvg track =
    let
        conns =
            Layout.getConnectors track

        from =
            Layout.getPosition conns.from

        to =
            Layout.getPosition conns.to
    in
    line
        [ from.x |> String.fromFloat |> x1
        , from.y |> String.fromFloat |> y1
        , to.x |> String.fromFloat |> x2
        , to.y |> String.fromFloat |> y2
        , stroke "black"
        , strokeLinecap "round"
        ]
        []


connectorToSvg : Layout.Connector -> Svg Msg
connectorToSvg conn =
    let
        pos =
            Layout.getPosition conn
    in
    circle
        [ pos.x |> String.fromFloat |> cx
        , pos.y |> String.fromFloat |> cy
        , r "5"
        , fill "none"
        , stroke "grey"
        , strokeWidth "1"
        ]
        []


trainToSvg : RR.Train -> Svg Msg
trainToSvg train =
    g [] (List.map trackOccupancyToSvg (RR.tracksForTrain train))


trackOccupancyToSvg : RR.TrackOccupancy -> Svg msg
trackOccupancyToSvg occ =
    let
        tl =
            Layout.trackLength occ.track

        conns =
            Layout.getConnectors occ.track

        from =
            Layout.getPosition conns.from

        to =
            Layout.getPosition conns.to

        dx =
            to.x - from.x

        dy =
            to.y - from.y
    in
    line
        [ from.x + dx * occ.from / tl |> String.fromFloat |> x1
        , from.y + dy * occ.from / tl |> String.fromFloat |> y1
        , from.x + dx * occ.to / tl |> String.fromFloat |> x2
        , from.y + dy * occ.to / tl |> String.fromFloat |> y2
        , stroke "red"
        , strokeLinecap "round"
        , strokeWidth "5"
        ]
        []


buttonGroup content =
    div [ class "btn-group", attribute "role" "group" ] content
