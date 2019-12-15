module Railroad exposing (Layout, Track, sample)


type alias Track =
    { from : Connector, to : Connector }


type alias Connector =
    { position : Position }


type alias Layout =
    { tracks : List Track }


type alias Position =
    { x : Int, y : Int }


sample : Layout
sample =
    let
        c1 =
            Connector (Position 100 100)
    in
    { tracks =
        [ Track c1 (Connector (Position 200 120))
        , Track c1 (Connector (Position 80 50))
        ]
    }
