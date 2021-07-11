module Cursor exposing
    ( Cursor
    , HomCoords
    , Transformation
    , apply
    , compose
    , cursorToTransformation
    , homCoords
    , homCoordsToXY
    , identityTransformation
    , rotate
    , translate
    )


type alias Cursor =
    { x : Float, y : Float, dir : Float }


type HomCoords
    = HC Float Float Float


type Transformation
    = T Float Float Float Float Float Float Float Float Float


homCoords : Cursor -> HomCoords
homCoords cursor =
    HC cursor.x cursor.y 1


homCoordsToXY : HomCoords -> ( Float, Float )
homCoordsToXY (HC x y w) =
    ( x / w, y / w )


identityTransformation : Transformation
identityTransformation =
    T 1 0 0 0 1 0 0 0 1


cursorToTransformation : Cursor -> Transformation
cursorToTransformation cursor =
    identityTransformation |> compose (translate cursor.x cursor.y) |> compose (rotate cursor.dir)


translate : Float -> Float -> Transformation
translate tx ty =
    T 1 0 tx 0 1 ty 0 0 1


rotate : Float -> Transformation
rotate a =
    T (cos a) (negate (sin a)) 0 (sin a) (cos a) 0 0 0 1


compose : Transformation -> Transformation -> Transformation
compose (T a00 a01 a02 a10 a11 a12 a20 a21 a22) (T b00 b01 b02 b10 b11 b12 b20 b21 b22) =
    T
        (a00 * b00 + a01 * b10 + a02 * b20)
        (a00 * b01 + a01 * b11 + a02 * b21)
        (a00 * b02 + a01 * b12 + a02 * b22)
        (a10 * b00 + a11 * b10 + a12 * b20)
        (a10 * b01 + a11 * b11 + a12 * b21)
        (a10 * b02 + a11 * b12 + a12 * b22)
        (a20 * b00 + a21 * b10 + a22 * b20)
        (a20 * b01 + a21 * b11 + a22 * b21)
        (a20 * b02 + a21 * b12 + a22 * b22)


apply : HomCoords -> Transformation -> HomCoords
apply (HC x y w) (T a00 a01 a02 a10 a11 a12 a20 a21 a22) =
    HC
        (a00 * x + a01 * y + a02 * w)
        (a10 * x + a11 * y + a12 * w)
        (a20 * x + a21 * y + a22 * w)
