module LayoutView exposing (Model, init, view)

import Html exposing (Html)
import Svg exposing (Svg, rect, svg)
import Svg.Attributes as SA


type alias Model =
    { zoom : Float
    , offset : { x : Float, y : Float }
    , drag : Maybe DragState
    }


type alias DragState =
    { start : { x : Float, y : Float }
    , current : { x : Float, y : Float }
    }


init : Model
init =
    { zoom = 1.0
    , offset = { x = 0, y = 0 }
    , drag = Nothing
    }


view : List (Svg msg) -> Model -> Html msg
view elements model =
    svg
        [ SA.width "100%"
        , SA.viewBox "0 0 800 450"
        , SA.preserveAspectRatio "xMidYMid meet"
        ]
        [ rect [ SA.x "0", SA.y "0", SA.width "800", SA.height "450", SA.fill "#fffff0" ] []
        , Svg.g
            [ SA.id "layout"
            , SA.transform
                ("translate("
                    ++ String.fromFloat model.offset.x
                    ++ ","
                    ++ String.fromFloat model.offset.y
                    ++ ") "
                    ++ "scale("
                    ++ String.fromFloat model.zoom
                    ++ ")"
                )
            ]
            elements
        ]
