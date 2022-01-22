port module Query exposing (..)

import Json.Decode exposing (..)



-- PORTS


port sendQuery : Query -> Cmd msg


port queryResponseReceiver : (Json.Decode.Value -> msg) -> Sub msg



-- MODEL


type alias Query =
    { id : String
    , path : QueryPath
    , orderBy : Maybe OrderBy
    , whereElements : List Where
    , limit : Maybe Int
    , collectionGroup : Bool
    }


type alias QueryPath =
    List QueryPathElement


type alias QueryPathElement =
    String


type alias Where =
    { field : String
    , op : String
    , value : String
    }


type alias OrderBy =
    { field : String
    , dir : String
    }


type alias Doc =
    { id : String
    , data : Json.Decode.Value
    }


type alias Snapshot =
    { id : String
    , docs : List Doc
    }


mapQueryID : (String -> String) -> Query -> Query
mapQueryID m q =
    { q | id = m q.id }


segmentsToQueryID : List String -> String
segmentsToQueryID =
    String.join "/"


queryIDToSegments : String -> List String
queryIDToSegments id =
    String.split "/" id
        |> List.filter (\s -> s /= "")


prefixQueryID : String -> String -> String
prefixQueryID prefix queryID =
    prefix ++ "/" ++ queryID


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
