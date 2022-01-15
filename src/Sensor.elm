module Sensor exposing (Sensor, sensorDecoder)

import Json.Decode exposing (..)


type alias Sensor =
    { min : Int
    , max : Int
    }


sensorDecoder : Json.Decode.Decoder Sensor
sensorDecoder =
    Json.Decode.map2 Sensor
        (field "min" Json.Decode.int)
        (field "max" Json.Decode.int)
