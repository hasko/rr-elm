module Railroad exposing (RailroadState, stateToSvg, viewTrains)

import Html exposing (Html, div, table, td, text, tr)
import Html.Attributes exposing (class)
import Svg exposing (Svg, svg)


type alias RailroadState =
    { layout : String, trains : List Train }


type alias Train =
    String


stateToSvg : RailroadState -> Html msg
stateToSvg s =
    svg [] []


viewTrains : List Train -> Html msg
viewTrains trains =
    div []
        [ table [ class "table" ] (List.map viewTrain trains) ]


viewTrain : Train -> Html msg
viewTrain train =
    tr [] [ td [] [ text train ] ]
