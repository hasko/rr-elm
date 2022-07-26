module Rect exposing (..)
import Svg.Attributes exposing (origin)


type Rect
    = Rect Float Float Float Float


rectToString : Rect -> String
rectToString r =
    let
        (x, y) = origin r
    in
    [ x, y, width r, height r ] |> List.map String.fromFloat |> String.join " "


expand : Float -> Rect -> Rect
expand d (Rect x1 y1 x2 y2) =
    Rect (x1 - d) (y1 - d) (x2 + d) (y2 + d)

origin : Rect -> (Float, Float)
origin (Rect x1 y1 _ _) = (x1, y1)


width : Rect -> Float
width (Rect x1 _ x2 _) =
    x2 - x1


height : Rect -> Float
height (Rect _ y1 _ y2) =
    y2 - y1
