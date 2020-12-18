module Main exposing (Msg(..), main, update, view)

import Browser
import Dict exposing (Dict)
import Graph exposing (empty, insertData, insertEdge)
import Html exposing (Html, button, div, pre, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Maybe.Extra
import Railroad exposing (..)
import Set exposing (Set)
import Svg exposing (Svg, g, line, path, rect, svg)
import Svg.Attributes exposing (d, fill, height, rx, ry, stroke, strokeWidth, viewBox, width, x, x1, x2, y, y1, y2)
import Task
import Time
import Tuple


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { state : TrainState
    , layout : Layout
    , lastTick : Maybe Int
    , running : Bool
    }


type Msg
    = Tick Time.Posix
    | Start
    | Stop
    | Reset


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = TrainState "Happy Train" 30.0 10.0 0 40.0
      , layout = initialLayout
      , lastTick = Nothing
      , running = True
      }
    , Cmd.none
    )


initialLayout : Layout
initialLayout =
    Graph.empty
        |> insertEdge 0 1
        |> insertEdge 1 0
        |> insertData 0 (StraightTrack { length = 50.0 })
        |> insertData 1 (StraightTrack { length = 100.0 })


subscriptions _ =
    Time.every 200 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            if model.running then
                case model.lastTick of
                    Just lastMillis ->
                        let
                            newMillis =
                                Time.posixToMillis time

                            elapsedMillis =
                                toFloat (newMillis - lastMillis)
                        in
                        ( { model
                            | state = Railroad.move elapsedMillis model.layout model.state
                            , lastTick = Just newMillis
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( { model | lastTick = Just (Time.posixToMillis time) }, Cmd.none )

            else
                ( { model | lastTick = Just (Time.posixToMillis time) }, Cmd.none )

        Start ->
            ( { model | running = True }, Cmd.none )

        Stop ->
            ( { model | running = False }, Cmd.none )

        Reset ->
            let
                ( m, cmd ) =
                    init ()
            in
            ( { m | running = model.running }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ svg
            [ width "100%", height "50%", viewBox "0 0 100000 5000" ]
            [ layoutToSvg 0 (Cursor 0.0 0.0 0.0) model.layout
            , rect
                [ x (String.fromFloat (1000.0 * ((toFloat model.state.track * 50.0) + model.state.trackPosition - model.state.length)))
                , y "1005"
                , width (String.fromFloat (model.state.length * 1000.0))
                , height "2990"
                , rx "15"
                , ry "15"
                , fill "#3B3332"
                ]
                []
            ]
        , button [ onClick Start, style "margin" "12px 12px 12px 12px" ] [ text "Start" ]
        , button [ onClick Stop, style "margin" "12px 12px 12px 0" ] [ text "Stop" ]
        , button [ onClick Reset, style "margin" "12px 12px 12px 0" ] [ text "Reset" ]
        , pre []
            [ text
                ("{ name='"
                    ++ model.state.name
                    ++ "'\n, length="
                    ++ String.fromFloat model.state.length
                    ++ "\n, speed="
                    ++ String.fromFloat model.state.speed
                    ++ "\n, track="
                    ++ String.fromInt model.state.track
                    ++ "\n, trackPosition="
                    ++ String.fromFloat model.state.trackPosition
                    ++ "\n}"
                )
            ]
        ]


layoutToSvg : Int -> Cursor -> Layout -> Svg Msg
layoutToSvg track_id cursor layout =
    case Graph.getData track_id layout of
        Nothing ->
            g [] []

        Just track ->
            g [] [ trackToSvg track cursor ]


trackToSvg : Track -> Cursor -> Svg Msg
trackToSvg track cursor =
    let
        new_cursor =
            Railroad.moveCursor cursor track
    in
    case track of
        StraightTrack s ->
            rect [ x (floatToSvg cursor.x), y (floatToSvg cursor.y), width (floatToSvg s.length), height "1435", fill "gray", stroke "black", strokeWidth "125" ] []

        CurvedTrack c ->
            path
                [ d
                    ("M"
                        ++ floatToSvg cursor.x
                        ++ ","
                        ++ floatToSvg cursor.y
                        ++ " B"
                        ++ floatToSvg cursor.dir
                        ++ " a"
                        ++ floatToSvg c.radius
                        ++ " "
                        ++ floatToSvg c.radius
                        ++ " 0 "
                        ++ (if c.angle >= 180.0 then
                                "1"

                            else
                                "0"
                           )
                        ++ ",1 "
                        ++ floatToSvg new_cursor.x
                        ++ ","
                        ++ floatToSvg new_cursor.y
                    )
                , fill "gray"
                , stroke "black"
                , strokeWidth "1435"
                ]
                []


floatToSvg : Float -> String
floatToSvg f =
    String.fromInt (f * 1000 |> floor)
