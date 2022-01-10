port module Main exposing (..)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import DateUtil exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Decode exposing (..)
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


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , flags : Flags
    , zone : Time.Zone
    , time : Time.Posix
    , err : Maybe String
    , nodes : List Node
    , motors : List Motor
    , sensors : Dict String Sensor
    , lastWaterGivens : Dict String WaterGiven
    , lastSensorReadings : Dict String SensorReading
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
    , triggerSensorId : String
    }


type alias Sensor =
    { min : Int
    , max : Int
    }


type alias PlantInfo =
    { motor : Motor
    , sensor : Maybe Sensor
    , lastWaterGiven : Maybe WaterGiven
    , lastSensorReading : Maybe SensorReading
    }


type alias WaterGiven =
    { seconds : Int
    , start : Time.Posix
    }


type alias SensorReading =
    { value : Int
    , timestamp : Time.Posix
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
      , sensors = Dict.empty
      , lastWaterGivens = Dict.empty
      , lastSensorReadings = Dict.empty
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
    List.concat (List.map queryForMotor motors)



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | Resized Int Int
    | SignIn
    | SignOut
    | ErrorParsingResponse String
    | QueryResponseReceived Json.Decode.Value
    | NodesReceived (List Node)
    | MotorsReceived (List Motor)
    | SensorReceived String (Maybe Sensor)
    | LastWaterGivenReceived String (Maybe WaterGiven)
    | LastSensorReadingReceived String (Maybe SensorReading)


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

        SensorReceived sensorID maybeSensor ->
            ( { model | sensors = Dict.update sensorID (\_ -> maybeSensor) model.sensors }
            , Cmd.none
            )

        LastWaterGivenReceived motorID maybeWaterGiven ->
            ( { model | lastWaterGivens = Dict.update motorID (\_ -> maybeWaterGiven) model.lastWaterGivens }
            , Cmd.none
            )

        LastSensorReadingReceived sensorID maybeSensorReading ->
            ( { model | lastSensorReadings = Dict.update sensorID (\_ -> maybeSensorReading) model.lastSensorReadings }
            , Cmd.none
            )


setSize : Int -> Int -> Flags -> Flags
setSize w h flags =
    { flags | width = w, height = h }


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


motorDecoder : Json.Decode.Decoder Motor
motorDecoder =
    Json.Decode.map6 Motor
        (field "id" Json.Decode.string)
        (field "parentID" Json.Decode.string)
        (field "name" Json.Decode.string)
        (field "type" Json.Decode.string)
        (field "enabled" Json.Decode.bool)
        (field "triggerSensorId" Json.Decode.string)


sensorDecoder : Json.Decode.Decoder Sensor
sensorDecoder =
    Json.Decode.map2 Sensor
        (field "min" Json.Decode.int)
        (field "max" Json.Decode.int)


waterGivenDecoder : Json.Decode.Decoder WaterGiven
waterGivenDecoder =
    Json.Decode.map2 WaterGiven
        (field "done" Json.Decode.int)
        (field "start" posixDecoder)


sensorReadingDecoder : Json.Decode.Decoder SensorReading
sensorReadingDecoder =
    Json.Decode.map2 SensorReading
        (field "moisture" Json.Decode.int)
        (field "timestamp" posixDecoder)


posixDecoder : Json.Decode.Decoder Time.Posix
posixDecoder =
    Json.Decode.int
        |> Json.Decode.map (\s -> Time.millisToPosix (s * 1000))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , Browser.Events.onResize (\w h -> Resized w h)
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
                , plantsView model.zone model.time (modelToPlantInfos model)
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


modelToPlantInfos : Model -> List PlantInfo
modelToPlantInfos model =
    List.map (\motor -> motorToPlantInfo motor model) model.motors


motorToPlantInfo motor model =
    { motor = motor
    , sensor = Dict.get motor.triggerSensorId model.sensors
    , lastWaterGiven = Dict.get motor.id model.lastWaterGivens
    , lastSensorReading = Dict.get motor.triggerSensorId model.lastSensorReadings
    }


errView : Maybe String -> Element Msg
errView maybeErr =
    case maybeErr of
        Nothing ->
            Element.none

        Just err ->
            Element.text err


plantsView : Time.Zone -> Time.Posix -> List PlantInfo -> Element Msg
plantsView zone time plantInfos =
    Element.column [ padding 10, spacing 20, width fill, height fill ] <|
        List.map (plantView zone time) plantInfos


lightBlue =
    rgb255 171 211 246


tileBGColor =
    lightBlue


plantView : Time.Zone -> Time.Posix -> PlantInfo -> Element Msg
plantView zone time plantInfo =
    Element.column
        [ Background.color tileBGColor
        , Font.color (rgb255 0 0 0)
        , Border.rounded 10
        , padding 20
        , spacing 10
        , width (fill |> minimum 250 |> maximum 800)
        , centerX
        ]
        [ plantViewHeader plantInfo
        , plantViewBody zone time plantInfo
        ]


plantViewHeader : PlantInfo -> Element Msg
plantViewHeader plantInfo =
    Element.el [ Font.bold ] (Element.text (plantInfo.motor.name ++ " | " ++ plantInfo.motor.nodeID))


plantViewBody : Time.Zone -> Time.Posix -> PlantInfo -> Element Msg
plantViewBody zone time plantInfo =
    Element.row [ spacing 30, width fill ]
        [ Element.el [ width (fillPortion 7), height fill ] <| plantViewBodyLeft zone time plantInfo
        , Element.el [ width (fillPortion 3), height fill ] <| plantViewBodyRight zone time plantInfo
        ]


plantViewBodyLeft : Time.Zone -> Time.Posix -> PlantInfo -> Element Msg
plantViewBodyLeft zone time plantInfo =
    Element.column [ spacing 5 ]
        [ motorLastWateredLabel zone time plantInfo.lastWaterGiven
        , lastReadingLabel zone time plantInfo.sensor plantInfo.lastSensorReading
        ]


plantViewBodyRight : Time.Zone -> Time.Posix -> PlantInfo -> Element Msg
plantViewBodyRight _ _ plantInfo =
    case Maybe.map2 readingToPercent plantInfo.sensor plantInfo.lastSensorReading of
        Nothing ->
            moistureBar 0 ""

        Just percent ->
            moistureBar percent (String.fromInt percent ++ "%")


readingToPercent : Sensor -> SensorReading -> Int
readingToPercent s r =
    clamp 0 100 <|
        100
            * (r.value - s.min)
            // (s.max - s.min)


moistureBar : Int -> String -> Element Msg
moistureBar percent title =
    Element.row [ width fill, height (fill |> minimum 100) ]
        [ Element.el
            [ width (fill |> maximum 100)
            , height fill
            , alignRight
            , centerY
            , Border.width 1
            , Background.gradient { angle = 0, steps = [ rgb255 91 58 0, rgb255 20 203 0 ] }
            , behindContent (partialBar percent tileBGColor)
            ]
            (Element.el [ centerX, centerY ] (Element.text title))
        ]


partialBar : Int -> Color -> Element Msg
partialBar percent backgroundColor =
    Element.column [ width fill, height fill ]
        [ Element.el [ width fill, height (fillPortion (100 - percent)), Background.color backgroundColor ] Element.none
        , Element.el [ width fill, height (fillPortion percent) ] Element.none
        ]


motorLastWateredLabel : Time.Zone -> Time.Posix -> Maybe WaterGiven -> Element Msg
motorLastWateredLabel zone time maybeWaterGiven =
    Element.paragraph []
        [ Element.text "Last watered: "
        , Element.text (formatWaterGiven zone time maybeWaterGiven)
        ]


formatWaterGiven : Time.Zone -> Time.Posix -> Maybe WaterGiven -> String
formatWaterGiven zone time maybeWaterGiven =
    case maybeWaterGiven of
        Nothing ->
            "Never"

        Just waterGiven ->
            DateUtil.humaneTimeSince zone time waterGiven.start ++ " for " ++ DateUtil.durationConcise waterGiven.seconds


lastReadingLabel : Time.Zone -> Time.Posix -> Maybe Sensor -> Maybe SensorReading -> Element Msg
lastReadingLabel zone time maybeSensor maybeSensorReading =
    Element.paragraph []
        [ Element.text "Last reading: "
        , Element.text (formatSensorReading zone time maybeSensor maybeSensorReading)
        ]


formatSensorReading : Time.Zone -> Time.Posix -> Maybe Sensor -> Maybe SensorReading -> String
formatSensorReading zone time maybeSensor maybeSensorReading =
    case maybeSensorReading of
        Nothing ->
            "missing"

        Just sensorReading ->
            formatSensorReading2 maybeSensor sensorReading ++ " (" ++ humaneTimeSince zone time sensorReading.timestamp ++ ")"


formatSensorReading2 : Maybe Sensor -> SensorReading -> String
formatSensorReading2 maybeSensor sensorReading =
    case maybeSensor of
        Nothing ->
            String.fromInt sensorReading.value

        Just sensor ->
            String.fromInt (clamp 0 100 (100 * (sensorReading.value - sensor.min) // (sensor.max - sensor.min))) ++ "%"


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
