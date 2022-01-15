module SensorReading exposing (SensorReading, sensorReadingDecoder)

import DateUtil exposing (posixDecoder)
import Json.Decode exposing (..)
import Time


type alias SensorReading =
    { value : Int
    , timestamp : Time.Posix
    }


sensorReadingDecoder : Json.Decode.Decoder SensorReading
sensorReadingDecoder =
    Json.Decode.map2 SensorReading
        (field "moisture" Json.Decode.int)
        (field "timestamp" posixDecoder)
