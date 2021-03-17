module Rect exposing (..)


type Rect
    = Rect Float Float Float Float


rectToString : Rect -> String
rectToString ((Rect x1 y1 x2 y2) as r) =
    [ x1, y1, width r, height r ] |> List.map String.fromFloat |> String.join " "


expand : Float -> Rect -> Rect
expand d (Rect x1 y1 x2 y2) =
    Rect (x1 - d) (y1 - d) (x2 + d) (y2 + d)


width : Rect -> Float
width (Rect x1 y1 x2 y2) =
    x2 - x1


height : Rect -> Float
height (Rect x1 y1 x2 y2) =
    y2 - y1
