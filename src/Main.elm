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
    ( { state = TrainState "Happy Train" 7.83 10.0 0 40.0
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
        |> insertEdge 1 2
        |> insertEdge 2 0
        |> insertData 0 (StraightTrack { length = 50.0 })
        |> insertData 1 (CurvedTrack { radius = 190.0, angle = 15.0 })
        |> insertData 2 (CurvedTrack { radius = 190.0, angle = 15.0 })


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
            [ width "100%", viewBox "0 0 150 50" ]
            [ layoutToSvg 0 (Cursor 0.0 2.5 0.0) model.layout Set.empty
            , rect
                [ x (String.fromFloat ((toFloat model.state.track * 50.0) + model.state.trackPosition - model.state.length))
                , y "1.005"
                , width (String.fromFloat model.state.length)
                , height "2.990"
                , rx "0.5"
                , ry "0.5"
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


layoutToSvg : Int -> Cursor -> Layout -> Set Int -> Svg Msg
layoutToSvg track_id cursor layout rendered =
    if Set.member track_id rendered then
        g [] []

    else
        case Graph.getData track_id layout of
            Nothing ->
                g [] []

            Just track ->
                let
                    ( svg, newCursor ) =
                        trackToSvg track cursor
                in
                g []
                    (svg
                        :: (Graph.outgoing track_id layout
                                |> Set.toList
                                |> List.map (\i -> layoutToSvg i newCursor layout (Set.insert track_id rendered))
                           )
                    )


trackToSvg : Track -> Cursor -> ( Svg Msg, Cursor )
trackToSvg track cursor =
    let
        newCursor =
            Railroad.moveCursor cursor track
    in
    ( case track of
        StraightTrack s ->
            line
                [ x1 (cursor.x |> String.fromFloat)
                , y1 (cursor.y |> String.fromFloat)
                , x2 (newCursor.x |> String.fromFloat)
                , y2 (newCursor.y |> String.fromFloat)
                , stroke "blue"
                , strokeWidth "1.435"
                ]
                []

        CurvedTrack c ->
            path
                [ d
                    ("M "
                        ++ (cursor.x |> String.fromFloat)
                        ++ " "
                        ++ (cursor.y |> String.fromFloat)
                        ++ " A "
                        ++ (c.radius |> String.fromFloat)
                        ++ " "
                        ++ (c.radius |> String.fromFloat)
                        ++ " 0 "
                        ++ (if c.angle >= 180.0 then
                                "1"

                            else
                                "0"
                           )
                        ++ " 1 "
                        ++ (newCursor.x |> String.fromFloat)
                        ++ " "
                        ++ (newCursor.y |> String.fromFloat)
                    )
                , fill "none"
                , stroke "green"
                , strokeWidth "1.435"
                ]
                []
    , newCursor
    )
