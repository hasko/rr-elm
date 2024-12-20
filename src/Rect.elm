module Rect exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SA


type Rect
    = Rect Float Float Float Float


rectToString : Rect -> String
rectToString r =
    let
        ( x, y ) =
            origin r
    in
    [ x, y, width r, height r ] |> List.map String.fromFloat |> String.join " "


rectToAttrib : Rect -> List (Svg.Attribute msg)
rectToAttrib (Rect x1 y1 x2 y2) =
    [ x1 |> String.fromFloat |> SA.x
    , y1 |> String.fromFloat |> SA.y
    , x2 - x1 + 1 |> String.fromFloat |> SA.width
    , y2 - y1 + 1 |> String.fromFloat |> SA.height
    ]


expand : Float -> Rect -> Rect
expand d (Rect x1 y1 x2 y2) =
    Rect (x1 - d) (y1 - d) (x2 + d) (y2 + d)


origin : Rect -> ( Float, Float )
origin (Rect x1 y1 _ _) =
    ( x1, y1 )


width : Rect -> Float
width (Rect x1 _ x2 _) =
    x2 - x1


height : Rect -> Float
height (Rect _ y1 _ y2) =
    y2 - y1
