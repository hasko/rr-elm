module Railroad exposing (Layout, RailroadState, Switch, layoutDecoder, loadedLayout, stateToSvg, toggleSwitch, viewTracks, viewTrains)

import Graph exposing (Graph)
import Html exposing (Html, button, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled)
import Html.Entity exposing (nbsp)
import Html.Events exposing (onClick)
import Json.Decode as JD exposing (Decoder, field, float, string)
import Svg exposing (Svg, svg)


type alias RailroadState =
    { layout : Layout, trains : List Train }


type alias Layout =
    { name : String, tracks : List Track, connections : List Connector, switches : List Switch }


type alias Track =
    { name : String, length : Float }


type alias Connector =
    { from : String, to : String }


type alias Switch =
    { name : String, connections : List Connector, state : Int }


type alias Train =
    { name : String, length : Float, speed : Float }


stateToSvg : RailroadState -> Html msg
stateToSvg s =
    svg [] []


viewTrains : RailroadState -> Html msg
viewTrains state =
    table [ class "table table-sm" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Length" ]
                , th [] [ text "Tracks" ]
                , th [] [ text "Orientation" ]
                , th [] [ text "Speed" ]
                ]
            ]
        , tbody []
            (List.map
                (\t ->
                    tr []
                        [ td [] [ text t.name ]
                        , td [] [ text (String.fromFloat t.length ++ nbsp ++ "m") ]
                        , td [] [ text nbsp ]
                        , td [] [ text nbsp ]
                        , td [] [ text (String.fromFloat (t.speed * 3.6) ++ nbsp ++ "km/h") ]
                        ]
                )
                state.trains
            )
        ]


viewTracks : RailroadState -> Html msg
viewTracks state =
    table [ class "table table-sm" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Length" ]
                , th [] [ text "Occupied by" ]
                ]
            ]
        , tbody []
            (List.map
                (\t ->
                    tr []
                        [ td [] [ text t.name ]
                        , td [] [ text (String.fromFloat t.length ++ nbsp ++ "m") ]
                        , td [] [ text <| String.join ", " <| List.map .name <| occupants state t.name ]
                        ]
                )
                state.layout.tracks
            )
        ]


trackDecoder : Decoder Track
trackDecoder =
    JD.map2 Track (field "name" string) (field "length" float)


connectionDecoder : Decoder Connector
connectionDecoder =
    JD.map2 Connector (field "from" string) (field "to" string)


switchDecoder : Decoder Switch
switchDecoder =
    JD.map3 Switch
        (field "name" string)
        (field "connections" (JD.list connectionDecoder))
        (JD.succeed 0)


layoutDecoder : Decoder Layout
layoutDecoder =
    JD.map4 Layout
        (field "name" string)
        (field "tracks" (JD.list trackDecoder))
        (field "connections" (JD.list connectionDecoder))
        (field "switches" (JD.list switchDecoder))


loadedLayout : Layout -> RailroadState
loadedLayout layout =
    { layout = layout, trains = [] }


occupants : RailroadState -> String -> List Train
occupants state trackName =
    []


toggleSwitch : RailroadState -> String -> Int -> RailroadState
toggleSwitch state switchName switchState =
    let
        l =
            state.layout
    in
    { state
        | layout =
            { l
                | switches =
                    List.map
                        (\s ->
                            if s.name == switchName then
                                { s | state = switchState }

                            else
                                s
                        )
                        l.switches
            }
    }
