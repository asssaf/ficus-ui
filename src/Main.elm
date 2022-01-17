port module Main exposing (..)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Element exposing (..)
import Header
import Json.Decode exposing (..)
import Log
import Login
import Plant
import Query
import Result.Extra as Result
import Task
import Time
import Url



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- PORTS


port sendQuery : Query.Query -> Cmd msg


port queryResponseReceiver : (Json.Decode.Value -> msg) -> Sub msg



-- MODEL


type alias Flags =
    { user : Maybe User
    , width : Int
    , height : Int
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , user : Maybe User
    , screen : ScreenModel
    , zone : Time.Zone
    , time : Time.Posix
    , log : Log.Model
    , nodes : List Node
    , plants : Plant.Model
    }


type alias ScreenModel =
    { width : Int
    , height : Int
    }


type alias User =
    { uid : String
    }


type alias Node =
    { id : String
    , name : String
    }


type QueryID
    = NodesQueryID
    | PlantQueryID Plant.QueryID


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , user = flags.user
      , screen = { width = flags.width, height = flags.height }
      , zone = Time.utc
      , time = Time.millisToPosix 0
      , log = Log.init
      , nodes = []
      , plants = Plant.init
      }
    , initCmd flags
    )


initCmd : Flags -> Cmd Msg
initCmd flags =
    case flags.user of
        Nothing ->
            Cmd.batch
                [ Task.perform AdjustTimeZone Time.here
                ]

        Just _ ->
            Cmd.batch
                [ Task.perform AdjustTimeZone Time.here
                , sendQuery nodeQuery
                , sendQuery Plant.motorsQuery
                ]


nodeQuery : Query.Query
nodeQuery =
    { id = Query.segmentsToQueryID [ "nodes" ]
    , path = [ "nodes" ]
    , whereElements = []
    , orderBy = Nothing
    , limit = Just 10
    , collectionGroup = False
    }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | Resized Int Int
    | QueryResponseReceived Json.Decode.Value
    | NodesReceived (List Node)
    | LogMsg Log.Msg
    | LoginMsg Login.Msg
    | HeaderMsg Header.Msg
    | PlantMsg Plant.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        Resized w h ->
            ( { model | screen = setSize w h model.screen }
            , Cmd.none
            )

        QueryResponseReceived response ->
            update (decodeQueryResponseAndExtract response) model

        NodesReceived nodes ->
            ( { model | nodes = nodes }
            , Cmd.none
            )

        HeaderMsg headerMsg ->
            ( model
            , Header.update headerMsg
            )

        LogMsg logMsg ->
            ( { model | log = Log.update logMsg model.log }
            , Cmd.none
            )

        LoginMsg loginMsg ->
            ( model
            , Login.update loginMsg
            )

        PlantMsg plantMsg ->
            let
                ( plants, queries ) =
                    Plant.update plantMsg model.plants
            in
            ( { model | plants = plants }
            , Cmd.batch <|
                List.map sendQuery queries
            )


setSize : Int -> Int -> ScreenModel -> ScreenModel
setSize w h screen =
    { screen | width = w, height = h }


decodeQueryResponseAndExtract : Json.Decode.Value -> Msg
decodeQueryResponseAndExtract response =
    Json.Decode.decodeValue queryResponseDecoder response
        |> Result.extract parseErrorToMessage


parseErrorToMessage : Json.Decode.Error -> Msg
parseErrorToMessage err =
    LogMsg <| Log.ErrorOccurred <| Json.Decode.errorToString err


queryResponseDecoder : Json.Decode.Decoder Msg
queryResponseDecoder =
    Query.snapshotDecoder |> Json.Decode.andThen snapshotToMessageDecoder


snapshotToMessageDecoder : Query.Snapshot -> Json.Decode.Decoder Msg
snapshotToMessageDecoder snapshot =
    case decodeQueryID snapshot.id of
        Nothing ->
            Json.Decode.fail <| "unknown query id: " ++ snapshot.id

        Just queryID ->
            queryIDToMessageDecoder queryID


decodeQueryID : String -> Maybe QueryID
decodeQueryID queryID =
    let
        segments =
            Query.queryIDToSegments queryID

        decoders =
            [ mainDecodeQueryID
            , Plant.decodeQueryID >> Maybe.map PlantQueryID
            ]
    in
    List.filterMap (\dec -> dec segments) decoders
        |> List.head


queryIDToMessageDecoder : QueryID -> Json.Decode.Decoder Msg
queryIDToMessageDecoder queryID =
    case queryID of
        NodesQueryID ->
            nodesQueryResponseDecoder

        PlantQueryID plantQueryID ->
            Plant.queryIDToMessageDecoder plantQueryID
                |> Json.Decode.map PlantMsg


mainDecodeQueryID : List String -> Maybe QueryID
mainDecodeQueryID segments =
    case segments of
        [ "nodes" ] ->
            Just NodesQueryID

        _ ->
            Nothing


nodesQueryResponseDecoder : Json.Decode.Decoder Msg
nodesQueryResponseDecoder =
    nodeDecoder
        |> Json.Decode.field "data"
        |> Json.Decode.list
        |> Json.Decode.field "docs"
        |> Json.Decode.map NodesReceived


nodeDecoder : Json.Decode.Decoder Node
nodeDecoder =
    Json.Decode.map2 Node
        (field "id" Json.Decode.string)
        (field "name" Json.Decode.string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , Browser.Events.onResize (\w h -> Resized w h)
        , queryResponseReceiver QueryResponseReceived
        , Log.subscriptions model.log |> Sub.map LogMsg
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.user of
        Nothing ->
            loginView model

        Just _ ->
            mainView model


loginView : Model -> Browser.Document Msg
loginView model =
    { title = "Ficus"
    , body =
        [ Element.layout [ width (px model.screen.width), height (px model.screen.height) ] <|
            Element.map LoginMsg <|
                Login.view
        ]
    }


mainView : Model -> Browser.Document Msg
mainView model =
    { title = "Ficus"
    , body =
        [ Element.layout [ width (px model.screen.width), height (px model.screen.height) ] <|
            Element.column [ width fill, height fill, spacing 20 ]
                [ Header.view |> Element.map HeaderMsg
                , Log.view model.log
                    |> Element.map LogMsg
                , Plant.view model.zone model.time model.plants
                    |> Element.map PlantMsg
                ]
        ]
    }
