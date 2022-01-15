module WaterGiven exposing (WaterGiven, waterGivenDecoder)

import DateUtil exposing (posixDecoder)
import Json.Decode exposing (..)
import Time


type alias WaterGiven =
    { seconds : Int
    , start : Time.Posix
    }


waterGivenDecoder : Json.Decode.Decoder WaterGiven
waterGivenDecoder =
    Json.Decode.map2 WaterGiven
        (field "done" Json.Decode.int)
        (field "start" posixDecoder)
