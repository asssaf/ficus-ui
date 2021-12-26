module Query exposing (..)

import Json.Decode exposing (..)


type alias Query =
    { id : String
    , path : QueryPath
    , limit : Int
    }


type alias QueryPath =
    List QueryPathElement


type alias QueryPathElement =
    String


type alias Doc =
    { id : String
    , data : Json.Decode.Value
    }


type alias Snapshot =
    { id : String
    , docs : List Doc
    }


snapshotDecoder : Json.Decode.Decoder Snapshot
snapshotDecoder =
    Json.Decode.map2 Snapshot
        (field "id" Json.Decode.string)
        (field "docs" (Json.Decode.list docDecoder))


docDecoder : Json.Decode.Decoder Doc
docDecoder =
    Json.Decode.map2 Doc
        (field "id" Json.Decode.string)
        (field "data" Json.Decode.value)
