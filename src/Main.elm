module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Dict exposing (Dict)
import Graph
import Html exposing (Html, br, button, div, li, pre, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, rowspan, scope, style)
import Html.Entity
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy2)
import Length exposing (Length)
import List.Extra
import Maybe exposing (andThen, withDefault)
import Quantity
import Railroad.Layout as Layout exposing (..)
import Railroad.Track as Track exposing (Track(..))
import Railroad.Train as Train exposing (..)
import Rect
import Round
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
    , switchState : Dict Int Int
    , running : Bool
    }


type Msg
    = Tick Float
    | Toggle
    | Reset
    | ChangeSwitch Int Switch


init : () -> ( Model, Cmd Msg )
init _ =
    let
        l =
            Layout.initialLayout
    in
    ( { state =
            { name = "Happy Train"
            , length = 30
            , speed = 10.0
            , location = Train.initialLocation l
            }
      , layout = l
      , switchState = Dict.empty
      , running = True
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        onAnimationFrameDelta Tick

    else
        Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            updateTick delta model

        Toggle ->
            ( { model | running = not model.running }, Cmd.none )

        Reset ->
            let
                ( m, cmd ) =
                    init ()
            in
            ( { m | running = False }, Cmd.none )

        ChangeSwitch i switch ->
            let
                -- Get the next configuration number, cycling around if necessary.
                newCfg =
                    modBy (List.length switch.configs) ((Dict.get i model.switchState |> withDefault 0) + 1)

                -- Get the new switch state for the model.
                newState =
                    Dict.insert i newCfg model.switchState
            in
            -- Construct the new model based on the new switch state.
            ( { model | switchState = newState }, Cmd.none )


updateTick : Float -> Model -> ( Model, Cmd Msg )
updateTick delta model =
    let
        newTrainState =
            Train.move model.layout model.switchState delta model.state
    in
    case newTrainState.location of
        Nothing ->
            -- Train could not move, stop it immediately.
            ( { model | state = Train.stopped model.state }, Cmd.none )

        Just loc ->
            ( { model | state = newTrainState }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ svg
            [ width "100%"
            , viewBox
                (model.layout
                    |> Layout.boundingBox
                    |> Rect.expand 5
                    |> Rect.rectToString
                )
            ]
            [ lazy Layout.toSvg model.layout
            , g [ id "trains" ] [ viewTrain model.state model.layout model.switchState ]
            ]
        , div [ class "row mb-3" ]
            [ div [ class "btn-group", role "group" ]
                [ button [ class "btn btn-primary me-3", onClick Toggle ]
                    [ text
                        (if model.running then
                            "Stop"

                         else
                            "Start"
                        )
                    ]
                , button [ class "btn btn-secondary", onClick Reset ] [ text "Reset" ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col" ] [ lazy2 viewSwitches model.layout model.switchState ]
            , div [ class "col" ]
                [ table [ class "table" ]
                    [ tbody []
                        [ tr [] [ th [ scope "row" ] [ text "Name" ], td [] [ text model.state.name ] ]
                        , tr [] [ th [ scope "row" ] [ text "Length" ], td [] [ text (String.fromFloat model.state.length ++ " m") ] ]
                        , tr []
                            [ th [ scope "row" ] [ text "Speed" ]
                            , td []
                                [ text
                                    (String.fromFloat model.state.speed
                                        ++ " m/s ("
                                        ++ Round.round 1 (model.state.speed * 3.6)
                                        ++ " km/h)"
                                    )
                                ]
                            ]
                        , tr []
                            [ th [ scope "row" ] [ text "Location" ]
                            , td []
                                (case model.state.location of
                                    Nothing ->
                                        [ text "Nowhere" ]

                                    Just loc ->
                                        [ text
                                            ("edge ("
                                                ++ String.fromInt (Tuple.first loc.edge)
                                                ++ ", "
                                                ++ String.fromInt (Tuple.second loc.edge)
                                                ++ ")"
                                            )
                                        , br [] []
                                        , text
                                            ("pos "
                                                ++ Round.round 2 (Length.inMeters loc.pos)
                                                ++ " m"
                                            )
                                        ]
                                )
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewTrain : TrainState -> Layout -> Dict Int Int -> Svg Msg
viewTrain train layout switchState =
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
                    case { loc | pos = loc.pos |> Quantity.minus (Length.meters train.length) } |> Train.normalizeLocation layout switchState of
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


viewSwitches : Layout -> Dict Int Int -> Html Msg
viewSwitches layout switchState =
    table [ class "table" ]
        [ thead [] [ tr [] [ th [] [ text "ID" ], th [] [ text "Connections" ], th [] [ text "Active" ], th [] [] ] ]
        , tbody []
            (Layout.switches layout |> List.map (\( i, switch ) -> viewSwitch i switch (Dict.get i switchState |> withDefault 0)))
        ]


viewSwitch : Int -> Switch -> Int -> Html Msg
viewSwitch i switch state =
    tr []
        [ td [] [ text (String.fromInt i) ]
        , td []
            [ ul [] (List.map viewSwitchConfig switch.configs |> List.map (\t -> li [] [ t ])) ]
        , td []
            [ case List.Extra.getAt state switch.configs of
                Nothing ->
                    -- Switch state is inconsistent
                    text "inconsistent"

                Just cfg ->
                    viewSwitchConfig cfg
            ]
        , td [] [ button [ class "btn btn-primary btn-sm", onClick (ChangeSwitch i switch) ] [ text "Change" ] ]
        ]


viewSwitchConfig : List ( Int, Int ) -> Html Msg
viewSwitchConfig routes =
    routes
        |> List.map (\( from, to ) -> String.fromInt from ++ Html.Entity.rarr ++ String.fromInt to)
        |> String.join ", "
        |> text



--- Utilities ---


role : String -> Html.Attribute msg
role r =
    Html.Attributes.attribute "role" r
