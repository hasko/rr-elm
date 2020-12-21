module Main exposing (Msg(..), main, update, view)

import Browser
import Dict exposing (Dict)
import Graph exposing (empty, insertData, insertEdgeData)
import Graph.Pair exposing (getEdgeData)
import Html exposing (Html, button, div, li, pre, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, style)
import Html.Entity
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Maybe exposing (andThen, withDefault)
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
    ( { state =
            { name = "Happy Train"
            , length = 30
            , speed = 10.0
            , location =
                case getEdgeData ( 0, 1 ) initialLayout of
                    Nothing ->
                        Nothing

                    Just track ->
                        Just { edge = ( 0, 1 ), pos = 40.0, track = track }
            }
      , layout = initialLayout
      , lastTick = Nothing
      , running = True
      }
    , Cmd.none
    )


initialLayout : Layout
initialLayout =
    Graph.empty
        |> insertEdgeData 0 1 (StraightTrack { length = 75.0 })
        |> insertEdgeData 1 2 (CurvedTrack { radius = 300.0, angle = 15.0 })
        |> insertEdgeData 1 3 (StraightTrack { length = 75.0 })
        |> insertData 1 (Switch [ [ ( 0, 2 ) ], [ ( 0, 3 ) ] ])


trackColor : ( Int, Int ) -> String
trackColor ( from, to ) =
    Dict.empty
        |> Dict.insert 0 (Dict.empty |> Dict.insert 1 "blue")
        |> Dict.insert 1 (Dict.empty |> Dict.insert 2 "green")
        |> Dict.insert 1 (Dict.empty |> Dict.insert 3 "orange")
        |> Dict.get from
        |> andThen (\d -> Dict.get to d)
        |> withDefault "grey"


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
                                newMillis - lastMillis

                            newTrainState =
                                Train.move model.layout elapsedMillis model.state
                        in
                        case newTrainState.location of
                            Nothing ->
                                -- Train could not move, stop it.
                                ( { model | state = Train.stopped model.state }, Cmd.none )

                            Just loc ->
                                ( { model
                                    | state = Train.move model.layout elapsedMillis model.state
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
            [ width "100%", viewBox "0 -2.5 160 15" ]
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
                    ++ "\n, location="
                    ++ (case model.state.location of
                            Nothing ->
                                "Nothing"

                            Just loc ->
                                "Just\n  { edge=("
                                    ++ String.fromInt (Tuple.first loc.edge)
                                    ++ ", "
                                    ++ String.fromInt (Tuple.second loc.edge)
                                    ++ ")\n  , pos="
                                    ++ String.fromFloat loc.pos
                                    ++ "\n  }"
                       )
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
                        , stroke (trackColor edge)
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
    case train.location of
        Nothing ->
            g [] []

        Just loc ->
            case Layout.coordsFor loc.pos loc.edge layout of
                -- If the coordinates are unknown ...
                Nothing ->
                    -- ... draw nothing.
                    g [] []

                Just c1 ->
                    case { loc | pos = loc.pos - train.length } |> Train.normalizeLocation layout of
                        Nothing ->
                            -- If the train end fits on no track, draw nothing.
                            g [] []

                        Just trainEndLocation ->
                            case Layout.coordsFor trainEndLocation.pos trainEndLocation.edge layout of
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
            (Layout.switches layout |> List.map (\( i, switch ) -> viewSwitch i switch))
        ]


viewSwitch : Int -> Switch -> Html Msg
viewSwitch i switch =
    tr []
        [ td [] [ text (String.fromInt i) ]
        , td []
            [ ul [] (List.map viewSwitchConfig switch.configs) ]
        , td [] []
        , td [] []
        ]


viewSwitchConfig : List ( Int, Int ) -> Html Msg
viewSwitchConfig routes =
    routes
        |> List.map (\( from, to ) -> String.fromInt from ++ Html.Entity.rarr ++ String.fromInt to)
        |> String.join ", "
        |> (\s -> li [] [ text s ])
