module Railroad.Train exposing
    ( Train
    , decoder
    , encode
    , endLocation
    , length
    , move
    , stopped
    )

import Array exposing (Array)
import Duration exposing (Duration)
import Frame2d
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Length exposing (Length)
import Point2d
import Quantity
import Railroad.Layout as Layout exposing (Layout, Location, nextTrack, previousTrack)
import Railroad.Orientation exposing (Orientation(..))
import Railroad.Track as Track
import Speed exposing (Speed)


type alias Train =
    { name : String
    , composition : List RollingStock
    , speed : Speed
    , location : Maybe Location
    }


type alias RollingStock =
    { length : Length }


length : { a | composition : List { b | length : Length } } -> Length
length train =
    List.map .length train.composition |> Quantity.sum


endLocation : Length -> Layout -> Array Int -> Location -> Maybe Location
endLocation l layout switchState startLoc =
    endLocationRec l Quantity.zero layout switchState startLoc


endLocationRec : Length -> Length -> Layout -> Array Int -> Location -> Maybe Location
endLocationRec l correction layout switchState startLoc =
    Layout.coordsFor startLoc.pos startLoc.edge layout
        |> Maybe.map Frame2d.originPoint
        |> Maybe.andThen
            (\p1 ->
                { startLoc | pos = startLoc.pos |> Quantity.minus l |> Quantity.minus correction }
                    |> normalizeLocation layout switchState
                    |> Maybe.andThen
                        (\loc ->
                            Layout.coordsFor loc.pos loc.edge layout
                                |> Maybe.map Frame2d.originPoint
                                |> Maybe.andThen
                                    (\p2 ->
                                        let
                                            d =
                                                Point2d.distanceFrom p1 p2

                                            {-
                                               err =
                                                   l |> Quantity.minus d

                                               _ =
                                                   Debug.log "Calculation" { length = Length.inMeters l, d = Length.inMeters d, err = Length.inMeters err }
                                            -}
                                        in
                                        if Quantity.equalWithin (Length.centimeters 5) d l then
                                            Just loc

                                        else
                                            endLocationRec l (Quantity.plus correction (l |> Quantity.minus d)) layout switchState startLoc
                                    )
                        )
            )


move : Duration -> Train -> Layout -> Array Int -> Train
move delta trainState layout switchState =
    case trainState.location of
        Nothing ->
            -- TODO If the train has no location, no need to move. Use off-map locations instead.
            trainState

        Just loc ->
            -- Calculate new position, disregarding track transitions.
            let
                distanceTraveled =
                    trainState.speed |> Quantity.for delta

                -- Consider which way the train is traveling on the track.
                newPos =
                    case loc.orientation of
                        Aligned ->
                            loc.pos |> Quantity.plus distanceTraveled

                        Reversed ->
                            loc.pos |> Quantity.minus distanceTraveled
            in
            -- Create a new train state ...
            { trainState
              -- ... but replace the location with a new location ...
                | location =
                    -- ... based on the old location but with an updated position ...
                    { loc | pos = newPos }
                        -- and finally "normalize" the location in case we are on a different track now.
                        |> normalizeLocation layout switchState
            }


normalizeLocation : Layout -> Array Int -> Location -> Maybe Location
normalizeLocation layout switchState loc =
    Layout.trackAt loc.edge layout
        |> Maybe.andThen
            (\track ->
                if loc.pos |> Quantity.greaterThan (Track.length track) then
                    nextTrack loc layout switchState
                        |> Maybe.andThen
                            (\nextLoc ->
                                normalizeLocation layout
                                    switchState
                                    { nextLoc
                                        | pos = loc.pos |> Quantity.minus (Track.length track)
                                    }
                            )

                else if Quantity.lessThanZero loc.pos then
                    previousTrack loc layout switchState
                        |> Maybe.andThen
                            (\nextLoc ->
                                Layout.trackAt nextLoc.edge layout
                                    |> Maybe.andThen
                                        (\nt ->
                                            normalizeLocation layout
                                                switchState
                                                { nextLoc | pos = loc.pos |> Quantity.plus (Track.length nt) }
                                        )
                            )

                else
                    Just loc
            )


stopped : Train -> Train
stopped ts =
    { ts | speed = Quantity.zero }



-- JSON


decoder : Decoder Train
decoder =
    Decode.map4 Train
        (Decode.field "name" Decode.string)
        (Decode.field "composition" (Decode.list rollingStockDecoder))
        (Decode.field "speed" (Decode.float |> Decode.map Speed.metersPerSecond))
        (Decode.field "location" (Decode.maybe Layout.locationDecoder))


rollingStockDecoder : Decoder RollingStock
rollingStockDecoder =
    Decode.map RollingStock
        (Decode.field "length" Decode.float
            |> Decode.map Length.meters
        )



-- JSON encode


encode : Train -> Value
encode ts =
    Encode.object
        [ ( "name", Encode.string ts.name )
        , ( "composition", Encode.list encodeRollingStock ts.composition )
        , ( "speed", ts.speed |> Speed.inMetersPerSecond |> Encode.float )
        , ( "loc"
          , case ts.location of
                Nothing ->
                    Encode.null

                Just loc ->
                    Layout.encodeLocation loc
          )
        ]


encodeRollingStock : RollingStock -> Value
encodeRollingStock rs =
    Encode.object [ ( "length", rs.length |> Length.inMeters |> Encode.float ) ]



-- JSON


edgeDecoder : Decoder ( Int, Int )
edgeDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "from" Decode.int)
        (Decode.field "to" Decode.int)


orientationDecoder : Decoder Orientation
orientationDecoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "aligned" ->
                        Decode.succeed Aligned

                    _ ->
                        Decode.fail "Invalid orientation"
            )
