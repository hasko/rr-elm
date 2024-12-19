port module Main exposing (Msg(..), main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Duration exposing (Duration)
import File.Download
import Html exposing (Html, a, br, button, div, h2, header, li, main_, nav, section, span, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, disabled, href, scope, style, type_)
import Html.Entity
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy2)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Length
import List.Extra
import Maybe exposing (withDefault)
import Quantity
import Railroad.Layout as Layout exposing (..)
import Railroad.Orientation as Orientation
import Railroad.Switch exposing (Switch)
import Railroad.Track exposing (Track(..))
import Railroad.Train as Train exposing (..)
import Railroad.Train.Svg
import Rect
import Round
import Set exposing (Set)
import Speed
import Svg exposing (g, svg)
import Svg.Attributes exposing (id, viewBox, width)
import Tuple


main : Program (Maybe Value) Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { trains : List Train
    , layout : Layout
    , switchState : Array Int
    , running : Bool
    , flash : Maybe String
    , deltas : List Duration
    }


type Msg
    = Tick Float
    | Toggle
    | Step
    | Reset
    | ChangeSwitch Int Switch
    | LayoutReceived Value
    | SaveRequested


port sendLayout : Value -> Cmd msg


port layoutReceiver : (Value -> msg) -> Sub msg


init : Maybe Value -> ( Model, Cmd Msg )
init _ =
    let
        l =
            Layout.initialLayout
    in
    ( { trains =
            [ { name = "Happy Train"
              , composition = List.repeat 5 { length = Length.meters 10 }
              , speed = Speed.kilometersPerHour 40.0
              , location = Just { edge = ( 0, 1 ), pos = Length.meters 55.0, orientation = Orientation.Aligned }
              }
            ]
      , layout = l
      , switchState = Array.repeat (Array.length l.switches) 0
      , running = False
      , flash = Nothing
      , deltas = []
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        onAnimationFrameDelta Tick

    else
        layoutReceiver LayoutReceived



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            let
                d =
                    Duration.milliseconds delta
            in
            ( updateTick d { model | deltas = d :: model.deltas |> List.take 100 }, Cmd.none )

        Toggle ->
            ( { model | running = not model.running }, Cmd.none )

        Step ->
            if model.running then
                ( model, Cmd.none )

            else
                ( updateTick Duration.second model, Cmd.none )

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
                    modBy (Array.length switch.configs) ((Array.get i model.switchState |> withDefault 0) + 1)

                -- Get the new switch state for the model.
                newState =
                    Array.set i newCfg model.switchState
            in
            -- Construct the new model based on the new switch state.
            ( { model | switchState = newState }, Cmd.none )

        LayoutReceived value ->
            case Decode.decodeValue modelDecoder value of
                Ok m ->
                    ( m, Cmd.none )

                Err err ->
                    ( { model | flash = Just (Decode.errorToString err) }, Cmd.none )

        SaveRequested ->
            ( model, encodeModel model |> Encode.encode 0 |> File.Download.string "rr.json" "application/json" )


updateTick : Duration -> Model -> Model
updateTick delta model =
    let
        newTrainStates =
            model.trains
                |> List.map
                    (\train ->
                        let
                            newTrainState =
                                Train.move delta train model.layout model.switchState
                        in
                        case newTrainState.location of
                            Nothing ->
                                -- Train could not move, stop it immediately.
                                Train.stopped train

                            Just loc ->
                                let
                                    invisible =
                                        Layout.tracksBefore loc (Train.length newTrainState) model.layout model.switchState
                                            |> List.all
                                                (\( _, _, t ) ->
                                                    case t of
                                                        MapExit ->
                                                            True

                                                        _ ->
                                                            False
                                                )
                                in
                                if invisible then
                                    { newTrainState | location = Nothing }

                                else
                                    newTrainState
                    )
    in
    { model
        | trains = newTrainStates
        , running = List.any (\train -> train.speed |> Quantity.greaterThanZero) newTrainStates
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header [ class "container" ]
            [ nav []
                [ ul []
                    [ li [] [ strong [] [ a [ href "#" ] [ text "Trains" ] ] ]
                    , li [] [ a [ href "#" ] [ text "Load" ] ]
                    , li [] [ a [ href "#", onClick SaveRequested ] [ text "Save" ] ]
                    ]
                , ul [] [ li [] [ text (String.fromInt (frameRate model.deltas) ++ " fps") ] ]
                ]
            ]
        , main_
            [ class "container" ]
            [ section [ id "layout" ]
                [ svg
                    [ width "100%"
                    , viewBox
                        (model.layout
                            |> Layout.boundingBox
                            |> Rect.expand 5
                            |> Rect.rectToString
                        )
                    ]
                    [ lazy2 Layout.toSvg model.layout model.switchState
                    , g [ id "trains" ] (List.map (\train -> Railroad.Train.Svg.toSvg train model.layout model.switchState) model.trains)
                    ]
                , div [ class "grid" ]
                    [ button [ class "primary", onClick Toggle ]
                        [ text
                            (if model.running then
                                "Stop"

                             else
                                "Start"
                            )
                        ]
                    , button [ class "secondary", onClick Step, disabled model.running ] [ text "Step (1s)" ]
                    , button [ class "secondary", onClick Reset ] [ text "Reset" ]
                    ]
                ]
            , section [ id "switches" ]
                [ h2 [] [ text "Switches" ]
                , viewSwitches model.layout model.switchState (getBlockedSwitches model.layout model.switchState model.trains)
                ]
            , section [ id "trains" ]
                [ h2 [] [ text "Trains" ]
                , viewTrains model
                ]
            ]
        ]


viewSwitches : Layout -> Array Int -> Set Int -> Html Msg
viewSwitches layout switchStates blockedSwitches =
    table []
        [ thead []
            [ tr []
                [ th [ scope "col" ] [ text "ID" ]
                , th [ scope "col" ] [ text "Connections" ]
                , th [ scope "col" ] [ text "Active" ]
                , th [ scope "col" ] []
                ]
            ]
        , tbody []
            (layout.switches
                |> Array.indexedMap
                    (\i switch ->
                        viewSwitch i
                            switch
                            (Array.get i switchStates |> withDefault 0)
                            (Set.member i blockedSwitches)
                    )
                |> Array.toList
            )
        ]


viewSwitch : Int -> Switch -> Int -> Bool -> Html Msg
viewSwitch i switch state blocked =
    tr []
        [ td [] [ text (String.fromInt i) ]
        , td []
            [ ul []
                (switch.configs
                    |> Array.map (viewSwitchRoutes switch.edges)
                    |> Array.map (\t -> li [ style "list-style" "none" ] [ t ])
                    |> Array.toList
                )
            ]
        , td []
            [ case Array.get state switch.configs of
                Nothing ->
                    -- Switch state is inconsistent
                    text "inconsistent"

                Just cfg ->
                    viewSwitchRoutes switch.edges cfg
            ]
        , td []
            [ button
                [ class "primary"
                , if blocked then
                    disabled True

                  else
                    onClick (ChangeSwitch i switch)
                ]
                [ text "Change" ]
            ]
        ]


viewSwitchRoutes : Array ( Int, Int ) -> List Int -> Html msg
viewSwitchRoutes edges activeEdges =
    activeEdges
        |> List.map (\e -> Array.get e edges)
        |> List.map viewSwitchRoute
        |> String.join ", "
        |> text


viewSwitchRoute : Maybe ( Int, Int ) -> String
viewSwitchRoute maybeEdge =
    case maybeEdge of
        Nothing ->
            "Undefined"

        Just ( from, to ) ->
            String.fromInt from ++ Html.Entity.rarr ++ String.fromInt to


viewTrains : Model -> Html Msg
viewTrains model =
    let
        ths =
            [ "Name", "Length", "Speed", "Location", "Tracks covered" ]

        tds =
            model.trains |> List.map (viewTrain model) |> List.Extra.transpose

        rows =
            List.Extra.zip ths tds
    in
    table
        []
        (rows
            |> List.map
                (\( h, ds ) ->
                    tr []
                        (th [ scope "row" ] [ text h ]
                            :: (ds |> List.map (\d -> td [] [ text d ]))
                        )
                )
        )


viewTrain : Model -> Train -> List String
viewTrain model train =
    [ train.name
    , String.fromFloat (Length.inMeters (Train.length train)) ++ " m"
    , Round.round 1 (Speed.inMetersPerSecond train.speed)
        ++ " m/s ("
        ++ Round.round 1 (Speed.inKilometersPerHour train.speed)
        ++ " km/h)"
    , case train.location of
        Nothing ->
            "Nowhere"

        Just loc ->
            ("edge ("
                ++ String.fromInt (Tuple.first loc.edge)
                ++ ", "
                ++ String.fromInt (Tuple.second loc.edge)
                ++ ")"
            )
                ++ (", pos "
                        ++ Round.round 2 (Length.inMeters loc.pos)
                        ++ " m"
                   )
                ++ ", "
                ++ Orientation.toString loc.orientation
    , train.location
        |> Maybe.map (\loc -> Layout.tracksBefore loc (Train.length train) model.layout model.switchState)
        |> Maybe.withDefault []
        |> List.map (\( from, to, _ ) -> String.fromInt from ++ Html.Entity.rarr ++ String.fromInt to)
        |> String.join ", "
    ]


getBlockedSwitches : Layout -> Array Int -> List Train -> Set Int
getBlockedSwitches layout switchStates trains =
    let
        coveredEdges =
            trains
                |> List.map
                    (\train ->
                        train.location
                            |> Maybe.map (\loc -> Layout.tracksBefore loc (Train.length train) layout switchStates)
                            |> Maybe.withDefault []
                    )
                |> List.concat
                |> List.map (\( from, to, _ ) -> ( from, to ))
                |> Set.fromList
    in
    layout.switches
        |> Array.toList
        |> List.indexedMap
            (\i sw ->
                ( i
                , sw.edges
                    |> Array.toList
                    |> Set.fromList
                    |> Set.intersect coveredEdges
                    |> Set.isEmpty
                    |> not
                )
            )
        |> List.filter Tuple.second
        |> List.map Tuple.first
        |> Set.fromList



--- Utilities ---


role : String -> Html.Attribute msg
role r =
    Html.Attributes.attribute "role" r


frameRate : List Duration -> Int
frameRate deltas =
    let
        avg =
            deltas |> Quantity.sum |> Duration.inSeconds
    in
    round (toFloat (List.length deltas) / avg)



--- JSON ---


modelDecoder : Decoder Model
modelDecoder =
    Decode.map3
        (\ts l sws ->
            { trains = ts
            , layout = l
            , switchState = sws
            , running = False
            , flash = Just "Loaded successfully"
            , deltas = []
            }
        )
        (Decode.field "trains" (Decode.list Train.decoder))
        (Decode.field "layout" Layout.decoder)
        (Decode.field "switchState" (Decode.array Decode.int))
        |> Decode.andThen
            (\m ->
                if Array.length m.switchState /= Array.length m.layout.switches then
                    Decode.fail "Incorrect number of switch states"

                else
                    Decode.succeed m
            )



-- JSON encode


encodeModel : Model -> Value
encodeModel model =
    Encode.object
        [ ( "layout", Layout.encode model.layout )
        , ( "trains", Encode.list Train.encode model.trains )
        , ( "switchStates", Encode.array Encode.int model.switchState )
        ]
