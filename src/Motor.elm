module Motor exposing (Motor, motorDecoder)

import Json.Decode exposing (..)


type alias Motor =
    { id : String
    , nodeID : String
    , name : String
    , typ : String
    , enabled : Bool
    , triggerSensorId : String
    }


motorDecoder : Json.Decode.Decoder Motor
motorDecoder =
    Json.Decode.map6 Motor
        (field "id" Json.Decode.string)
        (field "parentID" Json.Decode.string)
        (field "name" Json.Decode.string)
        (field "type" Json.Decode.string)
        (field "enabled" Json.Decode.bool)
        (field "triggerSensorId" Json.Decode.string)
