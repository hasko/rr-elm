module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, br, button, div, li, p, text, ul)
import Html.Attributes as Att exposing (attribute, disabled)
import Html.Events exposing (onClick)
import Railroad as RR
import Round
import Svg exposing (Svg, circle, g, line, svg)
import Svg.Attributes exposing (..)
import Time exposing (posixToMillis)


main =
    Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { initialState : RR.State
    , state : RR.State
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
                    if model.isRunning then
                        RR.moved duration model.state

                    else
                        model.state
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
                        [ g [] (List.map trackToSvg model.state.layout.tracks)
                        , g [] (List.map connectorToSvg (RR.connectors model.state.layout))
                        , g [] (List.map trainToSvg model.state.trains)
                        ]
                    ]
                ]
            , div [ class "row mt-3" ]
                [ div [ class "col" ]
                    [ if model.isRunning then
                        button [ class "btn btn-primary", onClick Stop ] [ text "Stop simulation" ]

                      else
                        button [ class "btn btn-primary", onClick Start ] [ text "Start simulation" ]
                    , button
                        [ class "btn btn-secondary ml-2"
                        , if model.isRunning then
                            disabled True

                          else
                            onClick Reset
                        ]
                        [ text "Reset" ]
                    ]
                ]
            ]
        ]
    }


scaleTransform : Float -> String
scaleTransform scale =
    let
        scaleStr =
            String.fromFloat scale
    in
    "scale(" ++ scaleStr ++ " " ++ scaleStr ++ ")"


trackToSvg : RR.Track -> Svg Msg
trackToSvg track =
    line
        [ track.from.pos.x |> String.fromFloat |> x1
        , track.from.pos.y |> String.fromFloat |> y1
        , track.to.pos.x |> String.fromFloat |> x2
        , track.to.pos.y |> String.fromFloat |> y2
        , stroke "black"
        , strokeLinecap "round"
        ]
        []


connectorToSvg : RR.Connector -> Svg Msg
connectorToSvg conn =
    circle
        [ conn.pos.x |> String.fromFloat |> cx
        , conn.pos.y |> String.fromFloat |> cy
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
            RR.trackLength occ.track

        dx =
            occ.track.to.pos.x - occ.track.from.pos.x

        dy =
            occ.track.to.pos.y - occ.track.from.pos.y
    in
    line
        [ occ.track.from.pos.x + dx * occ.from / tl |> String.fromFloat |> x1
        , occ.track.from.pos.y + dy * occ.from / tl |> String.fromFloat |> y1
        , occ.track.from.pos.x + dx * occ.to / tl |> String.fromFloat |> x2
        , occ.track.from.pos.y + dy * occ.to / tl |> String.fromFloat |> y2
        , stroke "red"
        , strokeLinecap "round"
        , strokeWidth "5"
        ]
        []


buttonGroup content =
    div [ class "btn-group", attribute "role" "group" ] content
