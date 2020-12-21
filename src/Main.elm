module Main exposing (Msg(..), main, update, view)

import Browser
import Dict exposing (Dict)
import Graph exposing (empty, insertEdgeData)
import Graph.Pair
import Html exposing (Html, button, div, pre, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, style)
import Html.Entity
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Maybe exposing (withDefault)
import Railroad.Layout as Layout exposing (..)
import Railroad.Train as Train exposing (..)
import Set exposing (Set)
import Svg exposing (Svg, g, line, path, rect, svg)
import Svg.Attributes exposing (d, fill, id, stroke, strokeWidth, viewBox, width, x1, x2, y1, y2)
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
    ( { state = TrainState "Happy Train" 30 10.0 ( 0, 1 ) 40.0
      , layout = initialLayout
      , lastTick = Nothing
      , running = True
      }
    , Cmd.none
    )


initialLayout : Layout
initialLayout =
    Graph.empty
        |> insertEdgeData 0 1 (StraightTrack { length = 50.0 })
        |> insertEdgeData 1 2 (CurvedTrack { radius = 300.0, angle = 15.0 })
        |> insertEdgeData 1 3 (StraightTrack { length = 100.0 })
        -- Add some behind the scenes connections.
        |> insertEdgeData 2 0 (StraightTrack { length = 200.0 })
        |> insertEdgeData 3 0 (StraightTrack { length = 200.0 })


subscriptions _ =
    Time.every 40 Tick


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
                            | state = Train.move elapsedMillis model.layout model.state
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
            [ width "100%", viewBox "0 -2.5 150 50" ]
            [ lazy viewLayout model.layout
            , viewTrain model.state model.layout
            ]
        , div [ class "row" ]
            [ div [ class "col-3" ]
                [ button [ class "btn btn-secondary", onClick Start, style "margin" "12px 12px 12px 12px" ] [ text "Start" ]
                , button [ class "btn btn-secondary", onClick Stop, style "margin" "12px 12px 12px 0" ] [ text "Stop" ]
                , button [ class "btn btn-secondary", onClick Reset, style "margin" "12px 12px 12px 0" ] [ text "Reset" ]
                ]
            , div [ class "col" ]
                [ viewSwitches model.layout
                ]
            ]
        , pre []
            [ text
                ("{ name='"
                    ++ model.state.name
                    ++ "'\n, length="
                    ++ String.fromFloat model.state.length
                    ++ "\n, speed="
                    ++ String.fromFloat model.state.speed
                    ++ "\n, track=("
                    ++ String.fromInt (Tuple.first model.state.track)
                    ++ ", "
                    ++ String.fromInt (Tuple.second model.state.track)
                    ++ ")\n, trackPosition="
                    ++ String.fromFloat model.state.trackPosition
                    ++ "\n}"
                )
            ]
        ]


viewLayout : Layout -> Svg Msg
viewLayout layout =
    -- Create a g element to contain the layout.
    g [ id "layout" ]
        -- Map the graph edges to SVG elements.
        (Graph.edges layout |> List.map (viewTrack layout))


viewTrack : Layout -> ( Int, Int ) -> Svg Msg
viewTrack layout edge =
    case Layout.renderInfo layout edge of
        Nothing ->
            -- Track cannot be rendered.
            g [] []

        Just ( c1, c2, track ) ->
            case track of
                StraightTrack s ->
                    line
                        [ x1 (c1.x |> String.fromFloat)
                        , y1 (c1.y |> String.fromFloat)
                        , x2 (c2.x |> String.fromFloat)
                        , y2 (c2.y |> String.fromFloat)
                        , stroke "blue"
                        , strokeWidth "1.435"
                        ]
                        []

                CurvedTrack c ->
                    path
                        [ d
                            ("M "
                                ++ (c1.x |> String.fromFloat)
                                ++ " "
                                ++ (c1.y |> String.fromFloat)
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
                                ++ (c2.x |> String.fromFloat)
                                ++ " "
                                ++ (c2.y |> String.fromFloat)
                            )
                        , fill "none"
                        , stroke "green"
                        , strokeWidth "1.435"
                        ]
                        []


viewTrain : TrainState -> Layout -> Svg Msg
viewTrain train layout =
    case Layout.coordsFor train.trackPosition train.track layout of
        -- Train head is not on any track
        Nothing ->
            g [] []

        Just c1 ->
            let
                trainEnd =
                    Train.normalizePosition { train | trackPosition = train.trackPosition - train.length } layout
            in
            case Layout.coordsFor trainEnd.trackPosition trainEnd.track layout of
                -- Train end is not on any track.
                Nothing ->
                    g [] []

                Just c2 ->
                    line
                        [ Svg.Attributes.id "train"
                        , x1 (c1.x |> String.fromFloat)
                        , y1 (c1.y |> String.fromFloat)
                        , x2 (c2.x |> String.fromFloat)
                        , y2 (c2.y |> String.fromFloat)
                        , stroke "#3B3332"
                        , strokeWidth "2.990"
                        ]
                        []


viewSwitches : Layout -> Html Msg
viewSwitches layout =
    table [ class "table" ]
        [ thead [] [ tr [] [ th [] [ text "ID" ], th [] [ text "Connections" ], th [] [ text "Active" ], th [] [] ] ]
        , tbody []
            (Layout.switches layout
                |> List.indexedMap
                    (\i conns ->
                        tr []
                            [ td [] [ text (String.fromInt i) ]
                            , td []
                                [ List.map (\( inc, out ) -> String.fromInt inc ++ Html.Entity.rarr ++ String.fromInt out) conns
                                    |> String.join ", "
                                    |> text
                                ]
                            , td [] []
                            , td [] []
                            ]
                    )
            )
        ]
