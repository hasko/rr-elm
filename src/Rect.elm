module Rect exposing (..)


type Rect
    = Rect Float Float Float Float


rectToString : Rect -> String
rectToString (Rect x1 y1 x2 y2) =
    [ x1, y1, x2, y2 ] |> List.map String.fromFloat |> String.join " "


expand : Float -> Rect -> Rect
expand d (Rect x1 y1 x2 y2) =
    Rect (x1 - d) (y1 - d) (x2 + d) (y2 + d)
