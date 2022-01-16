port module Main exposing (..)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Constants exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Json.Decode exposing (..)
import Motor exposing (Motor, motorDecoder)
import Plant
import Query
import Result.Extra as Result
import Sensor exposing (Sensor, sensorDecoder)
import SensorReading exposing (SensorReading, sensorReadingDecoder)
import Task
import Time
import Url
import WaterGiven exposing (WaterGiven, waterGivenDecoder)



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


port logReceiver : (Json.Decode.Value -> msg) -> Sub msg


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port sendQuery : Query.Query -> Cmd msg


port queryResponseReceiver : (Json.Decode.Value -> msg) -> Sub msg



-- MODEL


type alias Flags =
    { user : Maybe User
    , width : Int
    , height : Int
    }


type alias LogItem =
    { message : String
    , level : String
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , flags : Flags
    , zone : Time.Zone
    , time : Time.Posix
    , err : Maybe String
    , nodes : List Node
    , plants : Plant.Model
    }


type alias User =
    { uid : String
    }


type alias Node =
    { id : String
    , name : String
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
                , sendQuery motorQuery
                ]


nodeQuery : Query.Query
nodeQuery =
    { id = "nodes"
    , path = [ "nodes" ]
    , whereElements = []
    , orderBy = Nothing
    , limit = Just 10
    , collectionGroup = False
    }


motorQuery : Query.Query
motorQuery =
    { id = "motors"
    , path = [ "motors" ]
    , whereElements =
        [ { field = "visible"
          , op = "!="
          , value = "false"
          }
        ]
    , orderBy = Nothing
    , limit = Just 10
    , collectionGroup = True
    }


sensorQuery : String -> String -> Query.Query
sensorQuery nodeID sensorID =
    { id = "nodes/" ++ nodeID ++ "/sensors/" ++ sensorID
    , path = [ "nodes", nodeID, "sensors", sensorID ]
    , whereElements = []
    , orderBy = Nothing
    , limit = Nothing
    , collectionGroup = False
    }


lastDoneByDayQuery : String -> String -> Query.Query
lastDoneByDayQuery nodeID motorID =
    { id = "nodes/" ++ nodeID ++ "/motors/" ++ motorID ++ "/done-by-day/last"
    , path = [ "nodes", nodeID, "motors", motorID, "done-by-day" ]
    , whereElements = []
    , orderBy = Just { field = "start", dir = "desc" }
    , limit = Just 1
    , collectionGroup = False
    }


lastSensorReadingQuery : String -> String -> Query.Query
lastSensorReadingQuery nodeID sensorID =
    { id = "nodes/" ++ nodeID ++ "/sensors/" ++ sensorID ++ "/readings/last"
    , path = [ "nodes", nodeID, "sensors", sensorID, "readings" ]
    , whereElements = []
    , orderBy = Just { field = "timestamp", dir = "desc" }
    , limit = Just 1
    , collectionGroup = False
    }


queryForMotor : Motor -> List Query.Query
queryForMotor motor =
    [ lastDoneByDayQuery motor.nodeID motor.id
    , sensorQuery motor.nodeID motor.triggerSensorId
    , lastSensorReadingQuery motor.nodeID motor.triggerSensorId
    ]


queriesForMotors : List Motor -> List Query.Query
queriesForMotors motors =
    List.concatMap queryForMotor motors



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | Resized Int Int
    | LogItemReceived Json.Decode.Value
    | LogItemAdded LogItem
    | SignIn
    | SignOut
    | ErrorParsingResponse String
    | QueryResponseReceived Json.Decode.Value
    | NodesReceived (List Node)
    | MotorsReceived (List Motor)
    | SensorReceived String (Maybe Sensor)
    | LastWaterGivenReceived String (Maybe WaterGiven)
    | LastSensorReadingReceived String (Maybe SensorReading)
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
            ( { model | flags = setSize w h model.flags }
            , Cmd.none
            )

        LogItemReceived logItem ->
            update (decodeLogItemAndExtract logItem) model

        LogItemAdded logItem ->
            case logItem.level of
                "error" ->
                    update (ErrorParsingResponse logItem.message) model

                _ ->
                    ( model
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
            update (decodeQueryResponseAndExtract response) model

        ErrorParsingResponse err ->
            ( { model | err = Just err }
            , Cmd.none
            )

        NodesReceived nodes ->
            ( { model | nodes = nodes }
            , Cmd.none
            )

        MotorsReceived motors ->
            ( { model | plants = Plant.updateMotors motors model.plants }
            , Cmd.batch <|
                List.map sendQuery <|
                    queriesForMotors motors
            )

        SensorReceived sensorID maybeSensor ->
            ( { model | plants = Plant.updateSensor sensorID maybeSensor model.plants }
            , Cmd.none
            )

        LastWaterGivenReceived motorID maybeWaterGiven ->
            ( { model | plants = Plant.updateWaterGiven motorID maybeWaterGiven model.plants }
            , Cmd.none
            )

        LastSensorReadingReceived sensorID maybeSensorReading ->
            ( { model | plants = Plant.updateSensorReading sensorID maybeSensorReading model.plants }
            , Cmd.none
            )

        PlantMsg plantMsg ->
            ( { model | plants = Plant.update plantMsg model.plants }
            , Cmd.none
            )


setSize : Int -> Int -> Flags -> Flags
setSize w h flags =
    { flags | width = w, height = h }


decodeLogItemAndExtract : Json.Decode.Value -> Msg
decodeLogItemAndExtract response =
    Json.Decode.decodeValue logItemDecoder response
        |> Result.map LogItemAdded
        |> Result.extract parseErrorToMessage


logItemDecoder : Json.Decode.Decoder LogItem
logItemDecoder =
    Json.Decode.map2 LogItem
        (field "message" Json.Decode.string)
        (field "level" Json.Decode.string)


decodeQueryResponseAndExtract : Json.Decode.Value -> Msg
decodeQueryResponseAndExtract response =
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

        [ "nodes", _, "sensors", sensorID ] ->
            sensorDecoder
                |> Json.Decode.field "data"
                |> Json.Decode.list
                |> Json.Decode.field "docs"
                |> Json.Decode.map List.head
                |> Json.Decode.map (SensorReceived sensorID)

        [ "nodes", _, "motors", motorID, "done-by-day", "last" ] ->
            waterGivenDecoder
                |> Json.Decode.field "data"
                |> Json.Decode.list
                |> Json.Decode.field "docs"
                |> Json.Decode.map List.head
                |> Json.Decode.map (LastWaterGivenReceived motorID)

        [ "nodes", _, "sensors", sensorID, "readings", "last" ] ->
            sensorReadingDecoder
                |> Json.Decode.field "data"
                |> Json.Decode.list
                |> Json.Decode.field "docs"
                |> Json.Decode.map List.head
                |> Json.Decode.map (LastSensorReadingReceived sensorID)

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , Browser.Events.onResize (\w h -> Resized w h)
        , logReceiver LogItemReceived
        , queryResponseReceiver QueryResponseReceived
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.flags.user of
        Nothing ->
            loginView model

        Just _ ->
            mainView model


loginView : Model -> Browser.Document Msg
loginView model =
    { title = "Ficus"
    , body =
        [ Element.layout [ width (px model.flags.width), height (px model.flags.height) ] <|
            Element.column [ width fill, height fill ]
                [ Element.el [ width fill, height (px 200) ] signInButton ]
        ]
    }


mainView : Model -> Browser.Document Msg
mainView model =
    { title = "Ficus"
    , body =
        [ Element.layout [ width (px model.flags.width), height (px model.flags.height) ] <|
            Element.column [ width fill, height fill, spacing 20 ]
                [ headerView
                , errView model.err
                , Plant.view model.zone model.time model.plants
                    |> Element.map (\msg -> PlantMsg msg)
                ]
        ]
    }


headerView : Element Msg
headerView =
    Element.row
        [ padding 10
        , width fill
        , Font.color lightBlue
        , Background.color (rgb255 66 135 245)
        ]
        [ Element.el [ alignLeft ] (Element.text "Ficus")
        , Element.el [ alignRight ] signOutButton
        ]


errView : Maybe String -> Element Msg
errView maybeErr =
    case maybeErr of
        Nothing ->
            Element.none

        Just err ->
            Element.text err


signInButton =
    Input.button
        [ centerX, centerY ]
        { label = Element.text "Sign In"
        , onPress = Just SignIn
        }


signOutButton =
    Input.button
        []
        { label = Element.text "Sign Out"
        , onPress = Just SignOut
        }
