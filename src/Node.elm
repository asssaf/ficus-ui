module Node exposing (Node, nodeDecoder)

import Json.Decode exposing (..)


type alias Node =
    { id : String
    , name : String
    }


nodeDecoder : Json.Decode.Decoder Node
nodeDecoder =
    Json.Decode.map2 Node
        (field "id" Json.Decode.string)
        (field "name" Json.Decode.string)
