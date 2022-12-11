port module Main exposing (Msg(..), main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Dict exposing (Dict)
import Frame2d
import Html exposing (Html, a, br, button, div, li, nav, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, disabled, href, scope, type_)
import Html.Entity
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy2)
import Json.Encode exposing (Value)
import Length
import List.Extra
import Maybe exposing (withDefault)
import Point2d
import Railroad.Layout as Layout exposing (..)
import Railroad.Train as Train exposing (..)
import Rect
import Round
import Svg exposing (Svg, g, line, svg)
import Svg.Attributes exposing (id, stroke, strokeWidth, viewBox, width, x1, x2, y1, y2)
import Tuple


main : Program (Maybe Value) Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { trainState : TrainState
    , layout : Layout
    , switchState : Dict Int Int
    , running : Bool
    }


type Msg
    = Tick Float
    | Toggle
    | Step
    | Reset
    | ChangeSwitch Int Switch


port sendLayout : Value -> Cmd msg


port layoutReceiver : (Value -> msg) -> Sub msg


init : Maybe Value -> ( Model, Cmd Msg )
init _ =
    let
        l =
            Layout.initialLayout
    in
    ( { trainState =
            { name = "Happy Train"
            , composition = [ { length = Length.meters 10 }, { length = Length.meters 10 }, { length = Length.meters 10 } ]
            , speed = 10.0
            , location = Train.initialLocation l
            }
      , layout = l
      , switchState = Dict.empty
      , running = False
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

        Step ->
            if model.running then
                ( model, Cmd.none )

            else
                updateTick 1000.0 model

        Reset ->
            let
                ( m, _ ) =
                    init Nothing
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
            Train.move delta model.trainState model.layout model.switchState
    in
    case newTrainState.location of
        Nothing ->
            -- Train could not move, stop it immediately.
            ( { model | trainState = Train.stopped model.trainState, running = False }, Cmd.none )

        Just _ ->
            ( { model | trainState = newTrainState }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ nav [ class "navbar navbar-expand-lg" ]
            [ div [ class "container-fluid" ]
                [ a [ class "navbar-brand", href "#" ] [ text "Trains" ]
                , button [ class "navbar-toggler", type_ "button", attribute "data-bs-toggle" "collapse", attribute "data-bs-target" "#navbarSupportedContent" ]
                    [ span [ class "navbar-toggler-icon" ] []
                    ]
                , div [ class "collapse navbar-collapse", id "navbarSupportedContent" ]
                    [ ul [ class "navbar-nav me-auto mb-2 mb-lg-0" ]
                        [ li [ class "nav-item" ] [ a [ class "nav-link", href "#" ] [ text "Load" ] ] ]
                    ]
                ]
            ]
        , svg
            [ width "100%"
            , viewBox
                (model.layout
                    |> Layout.boundingBox
                    |> Rect.expand 5
                    |> Rect.rectToString
                )
            ]
            [ lazy Layout.toSvg model.layout
            , g [ id "trains" ] [ viewTrain model.trainState model.layout model.switchState ]
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
                , button [ class "btn btn-secondary me-3", onClick Step, disabled model.running ] [ text "Step (1s)" ]
                , button [ class "btn btn-secondary", onClick Reset ] [ text "Reset" ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col" ] [ lazy2 viewSwitches model.layout model.switchState ]
            , div [ class "col" ]
                [ table [ class "table" ]
                    [ tbody []
                        [ tr [] [ th [ scope "row" ] [ text "Name" ], td [] [ text model.trainState.name ] ]
                        , tr [] [ th [ scope "row" ] [ text "Length" ], td [] [ text (String.fromFloat (Length.inMeters (Train.length model.trainState)) ++ " m") ] ]
                        , tr []
                            [ th [ scope "row" ] [ text "Speed" ]
                            , td []
                                [ text
                                    (String.fromFloat model.trainState.speed
                                        ++ " m/s ("
                                        ++ Round.round 1 (model.trainState.speed * 3.6)
                                        ++ " km/h)"
                                    )
                                ]
                            ]
                        , tr []
                            [ th [ scope "row" ] [ text "Location" ]
                            , td []
                                (case model.trainState.location of
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
            g [ Svg.Attributes.id "train" ]
                (List.foldl
                    (\car ( currentLoc, svg ) ->
                        case Layout.coordsFor currentLoc.pos currentLoc.edge layout of
                            Nothing ->
                                ( currentLoc, svg )

                            Just c1 ->
                                case Train.endLocation car.length layout switchState currentLoc of
                                    Nothing ->
                                        -- If the train end fits on no track, draw nothing.
                                        ( currentLoc, svg )

                                    Just trainEndLocation ->
                                        case Layout.coordsFor trainEndLocation.pos trainEndLocation.edge layout of
                                            -- Train end is not on any track.
                                            Nothing ->
                                                ( currentLoc, svg )

                                            Just c2 ->
                                                let
                                                    p1 =
                                                        c1 |> Frame2d.originPoint |> Point2d.toRecord Length.inMeters

                                                    p2 =
                                                        c2 |> Frame2d.originPoint |> Point2d.toRecord Length.inMeters
                                                in
                                                ( trainEndLocation
                                                , line
                                                    [ x1 (p1.x |> String.fromFloat)
                                                    , y1 (p1.y |> String.fromFloat)
                                                    , x2 (p2.x |> String.fromFloat)
                                                    , y2 (p2.y |> String.fromFloat)
                                                    , stroke "#3B3332"
                                                    , strokeWidth "2.990"
                                                    ]
                                                    []
                                                    :: svg
                                                )
                    )
                    ( loc, [] )
                    train.composition
                    |> Tuple.second
                )


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
