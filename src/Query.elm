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
    }


type alias Snapshot =
    { docs : List Doc
    }


decodeSnapshot : Json.Decode.Value -> Snapshot
decodeSnapshot v =
    { docs = [] }
