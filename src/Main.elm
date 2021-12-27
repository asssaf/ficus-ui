port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import DateUtil exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Input as Input
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode exposing (..)
import Query exposing (..)
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


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port sendQuery : Query.Query -> Cmd msg


port queryResponseReceiver : (Json.Decode.Value -> msg) -> Sub msg



-- MODEL


type alias Flags =
    { user : Maybe User
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , flags : Flags
    , zone : Time.Zone
    , time : Time.Posix
    , err : Maybe String
    , nodes : List Node
    , motors : List Motor
    , lastWaterGivens : Dict String WaterGiven
    }


type alias User =
    { uid : String
    }


type alias Node =
    { id : String
    , name : String
    }


type alias Motor =
    { id : String
    , nodeID : String
    , name : String
    , typ : String
    , enabled : Bool
    }


type alias MotorInfo =
    { motor : Motor
    , lastWaterGiven : Maybe WaterGiven
    }


type alias WaterGiven =
    { seconds : Int
    , start : Time.Posix
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , flags = flags
      , zone = Time.utc
      , time = Time.millisToPosix 0
      , err = Nothing
      , nodes = []
      , motors = []
      , lastWaterGivens = Dict.empty
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

        Just user ->
            Cmd.batch
                [ Task.perform AdjustTimeZone Time.here
                , sendQuery nodeQuery
                , sendQuery motorQuery
                ]


nodeQuery : Query.Query
nodeQuery =
    { id = "nodes"
    , path = [ "nodes" ]
    , orderBy = Nothing
    , limit = 10
    , collectionGroup = False
    }


motorQuery : Query.Query
motorQuery =
    { id = "motors"
    , path = [ "motors" ]
    , orderBy = Nothing
    , limit = 10
    , collectionGroup = True
    }


lastDoneByDayQuery : String -> String -> Query.Query
lastDoneByDayQuery nodeID motorID =
    { id = "nodes/" ++ nodeID ++ "/motors/" ++ motorID ++ "/done-by-day/last"
    , path = [ "nodes", nodeID, "motors", motorID, "done-by-day" ]
    , orderBy = Just { field = "start", dir = "desc" }
    , limit = 1
    , collectionGroup = False
    }


queryForMotor : Motor -> Query.Query
queryForMotor motor =
    lastDoneByDayQuery motor.nodeID motor.id


queriesForMotors =
    List.map queryForMotor



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | SignIn
    | SignOut
    | ErrorParsingResponse String
    | QueryResponseReceived Json.Decode.Value
    | NodesReceived (List Node)
    | MotorsReceived (List Motor)
    | LastWaterGivenReceived String (Maybe WaterGiven)


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

        SignIn ->
            ( model
            , signIn ()
            )

        SignOut ->
            ( model
            , signOut ()
            )

        QueryResponseReceived response ->
            update (decodeAndExtract response) model

        ErrorParsingResponse err ->
            ( { model | err = Just err }
            , Cmd.none
            )

        NodesReceived nodes ->
            ( { model | nodes = nodes }
            , Cmd.none
            )

        MotorsReceived motors ->
            ( { model | motors = motors }
            , Cmd.batch (List.map sendQuery (queriesForMotors motors))
            )

        LastWaterGivenReceived motorID maybeWaterGiven ->
            ( { model | lastWaterGivens = Dict.update motorID (\v -> maybeWaterGiven) model.lastWaterGivens }
            , Cmd.none
            )


decodeAndExtract : Json.Decode.Value -> Msg
decodeAndExtract response =
    Json.Decode.decodeValue queryResponseDecoder response
        |> Result.extract parseErrorToMessage


parseErrorToMessage : Json.Decode.Error -> Msg
parseErrorToMessage err =
    ErrorParsingResponse (Json.Decode.errorToString err)


queryResponseDecoder : Json.Decode.Decoder Msg
queryResponseDecoder =
    Query.snapshotDecoder |> Json.Decode.andThen snapshotToMessageDecoder


snapshotToMessageDecoder : Query.Snapshot -> Json.Decode.Decoder Msg
snapshotToMessageDecoder snapshot =
    case queryIDToSegments snapshot.id of
        [ "nodes" ] ->
            nodeDecoder
                |> Json.Decode.field "data"
                |> Json.Decode.list
                |> Json.Decode.field "docs"
                |> Json.Decode.map NodesReceived

        [ "motors" ] ->
            motorDecoder
                |> Json.Decode.field "data"
                |> Json.Decode.list
                |> Json.Decode.field "docs"
                |> Json.Decode.map MotorsReceived

        [ "nodes", nodeID, "motors", motorID, "done-by-day", "last" ] ->
            waterGivenDecoder
                |> Json.Decode.field "data"
                |> Json.Decode.list
                |> Json.Decode.field "docs"
                |> Json.Decode.map List.head
                |> Json.Decode.map (LastWaterGivenReceived motorID)

        _ ->
            Json.Decode.fail ("unknown query id: " ++ snapshot.id)


queryIDToSegments : String -> List String
queryIDToSegments id =
    String.split "/" id
        |> List.filter (\s -> s /= "")


nodeDecoder : Json.Decode.Decoder Node
nodeDecoder =
    Json.Decode.map2 Node
        (field "id" Json.Decode.string)
        (field "name" Json.Decode.string)


motorDecoder : Json.Decode.Decoder Motor
motorDecoder =
    Json.Decode.map5 Motor
        (field "id" Json.Decode.string)
        (field "parentID" Json.Decode.string)
        (field "name" Json.Decode.string)
        (field "type" Json.Decode.string)
        (field "enabled" Json.Decode.bool)


waterGivenDecoder : Json.Decode.Decoder WaterGiven
waterGivenDecoder =
    Json.Decode.map2 WaterGiven
        (field "done" Json.Decode.int)
        (field "start" posixDecoder)


posixDecoder : Json.Decode.Decoder Time.Posix
posixDecoder =
    Json.Decode.int
        |> Json.Decode.map (\s -> Time.millisToPosix (s * 1000))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , queryResponseReceiver QueryResponseReceived
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.flags.user of
        Nothing ->
            loginView model

        Just user ->
            mainView model


loginView : Model -> Browser.Document Msg
loginView model =
    { title = "Ficus"
    , body =
        [ Element.layout [] <|
            Element.column []
                [ Element.el [ alignRight ] signInButton ]
        ]
    }


mainView : Model -> Browser.Document Msg
mainView model =
    { title = "Ficus"
    , body =
        [ Element.layout [] <|
            Element.column []
                [ Element.el [ alignRight ] signOutButton
                , errView model.err
                , nodesView model.nodes
                , motorsView model.zone model.time (modelToMotorInfos model)
                ]
        ]
    }


modelToMotorInfos : Model -> List MotorInfo
modelToMotorInfos model =
    List.map (\m -> { motor = m, lastWaterGiven = Dict.get m.id model.lastWaterGivens }) model.motors


errView : Maybe String -> Element Msg
errView maybeErr =
    case maybeErr of
        Nothing ->
            Element.text "no error"

        Just err ->
            Element.text err


nodesView : List Node -> Element Msg
nodesView nodes =
    Element.column []
        (List.map nodeView nodes)


nodeView : Node -> Element Msg
nodeView node =
    Element.text (node.id ++ " " ++ node.name)


motorsView : Time.Zone -> Time.Posix -> List MotorInfo -> Element Msg
motorsView zone time motorInfos =
    Element.column [] <|
        List.map (motorView zone time) motorInfos


motorView : Time.Zone -> Time.Posix -> MotorInfo -> Element Msg
motorView zone time motorInfo =
    Element.column []
        [ Element.text motorInfo.motor.name
        , motorLastWateredLabel zone time motorInfo.lastWaterGiven
        ]


motorLastWateredLabel : Time.Zone -> Time.Posix -> Maybe WaterGiven -> Element Msg
motorLastWateredLabel zone time maybeWaterGiven =
    Element.text
        ("Last watered: "
            ++ formatWaterGiven zone time maybeWaterGiven
        )


formatWaterGiven : Time.Zone -> Time.Posix -> Maybe WaterGiven -> String
formatWaterGiven zone time maybeWaterGiven =
    case maybeWaterGiven of
        Nothing ->
            "Never"

        Just waterGiven ->
            DateUtil.daysSinceHumane zone time waterGiven.start ++ " for " ++ String.fromInt waterGiven.seconds ++ " seconds"


signInButton =
    Input.button
        []
        { label = Element.text "Sign In"
        , onPress = Just SignIn
        }


signOutButton =
    Input.button
        []
        { label = Element.text "Sign Out"
        , onPress = Just SignOut
        }
