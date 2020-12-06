module Railroad exposing (Layout, RailroadState, Switch, createTrain, layoutDecoder, loadedLayout, occupants, stateToSvg, switchStateString, toggleSwitch)

import Graph exposing (Graph)
import Html exposing (Html)
import Html.Entity exposing (mdash)
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
    { name : String, length : Float, speed : Float, track : String }


stateToSvg : RailroadState -> Html msg
stateToSvg s =
    svg [] []


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


switchStateString : Switch -> String
switchStateString switch =
    List.indexedMap Tuple.pair switch.connections
        |> List.filter (\( i, _ ) -> i == switch.state)
        |> List.map (\( _, c ) -> c.from ++ mdash ++ c.to)
        |> String.join ", "


createTrain : RailroadState -> String -> RailroadState
createTrain state trackName =
    let
        mTrack =
            state.layout.tracks |> List.filter (\t -> t.name == trackName) |> List.head
    in
    case mTrack of
        Nothing ->
            state

        Just track ->
            let
                length =
                    min 20 track.length

                train =
                    Train "Unnamed Train" length 0.0 trackName
            in
            { state | trains = train :: state.trains }
