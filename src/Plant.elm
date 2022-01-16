module Plant exposing
    ( Model
    , Msg
    , PlantInfo
    , QueryID
    , decodeQueryID
    , init
    , motorsQuery
    , queryIDToMessageDecoder
    , update
    , view
    )

import CollectionUtil exposing (..)
import Constants exposing (..)
import DateUtil exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Json.Decode
import Motor exposing (Motor, motorDecoder)
import Query
import Sensor exposing (Sensor, sensorDecoder)
import SensorReading exposing (SensorReading, sensorReadingDecoder)
import Time
import WaterGiven exposing (WaterGiven, waterGivenDecoder)



-- MODEL


type alias Model =
    { motors : Dict String Motor
    , sensors : Dict String Sensor
    , lastWaterGivens : Dict String WaterGiven
    , lastSensorReadings : Dict String SensorReading
    }


type alias PlantInfo =
    { motor : Motor
    , sensor : Maybe Sensor
    , lastWaterGiven : Maybe WaterGiven
    , lastSensorReading : Maybe SensorReading
    }


type QueryID
    = MotorsQueryID
    | SensorQueryID String
    | LastWaterGivenQueryID String
    | LastSensorReadingQueryID String


motorsQuery : Query.Query
motorsQuery =
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


motorsQueryResponseDecoder =
    motorDecoder
        |> Json.Decode.field "data"
        |> Json.Decode.list
        |> Json.Decode.field "docs"
        |> Json.Decode.map MotorsReceived


queryForMotor : Motor -> List Query.Query
queryForMotor motor =
    [ lastWaterGivenQuery motor.nodeID motor.id
    , sensorQuery motor.nodeID motor.triggerSensorId
    , lastSensorReadingQuery motor.nodeID motor.triggerSensorId
    ]


queriesForMotors : List Motor -> List Query.Query
queriesForMotors motors =
    List.concatMap queryForMotor motors


sensorQuery : String -> String -> Query.Query
sensorQuery nodeID sensorID =
    { id = Query.segmentsToQueryID [ "nodes", nodeID, "sensors", sensorID ]
    , path = [ "nodes", nodeID, "sensors", sensorID ]
    , whereElements = []
    , orderBy = Nothing
    , limit = Nothing
    , collectionGroup = False
    }


sensorQueryResponseDecoder : String -> Json.Decode.Decoder Msg
sensorQueryResponseDecoder sensorID =
    sensorDecoder
        |> Json.Decode.field "data"
        |> Json.Decode.list
        |> Json.Decode.field "docs"
        |> Json.Decode.map List.head
        |> Json.Decode.map (SensorReceived sensorID)


lastWaterGivenQuery : String -> String -> Query.Query
lastWaterGivenQuery nodeID motorID =
    { id = Query.segmentsToQueryID [ "nodes", nodeID, "motors", motorID, "done-by-day", "last" ]
    , path = [ "nodes", nodeID, "motors", motorID, "done-by-day" ]
    , whereElements = []
    , orderBy = Just { field = "start", dir = "desc" }
    , limit = Just 1
    , collectionGroup = False
    }


lastWaterGivenQueryResponseDecoder : String -> Json.Decode.Decoder Msg
lastWaterGivenQueryResponseDecoder motorID =
    waterGivenDecoder
        |> Json.Decode.field "data"
        |> Json.Decode.list
        |> Json.Decode.field "docs"
        |> Json.Decode.map List.head
        |> Json.Decode.map (LastWaterGivenReceived motorID)


lastSensorReadingQuery : String -> String -> Query.Query
lastSensorReadingQuery nodeID sensorID =
    { id = Query.segmentsToQueryID [ "nodes", nodeID, "sensors", sensorID, "readings", "last" ]
    , path = [ "nodes", nodeID, "sensors", sensorID, "readings" ]
    , whereElements = []
    , orderBy = Just { field = "timestamp", dir = "desc" }
    , limit = Just 1
    , collectionGroup = False
    }


lastSensorReadingQueryResponseDecoder : String -> Json.Decode.Decoder Msg
lastSensorReadingQueryResponseDecoder sensorID =
    sensorReadingDecoder
        |> Json.Decode.field "data"
        |> Json.Decode.list
        |> Json.Decode.field "docs"
        |> Json.Decode.map List.head
        |> Json.Decode.map (LastSensorReadingReceived sensorID)


decodeQueryID : List String -> Maybe QueryID
decodeQueryID segments =
    case segments of
        [ "motors" ] ->
            Just MotorsQueryID

        [ "nodes", _, "sensors", sensorID ] ->
            Just <| SensorQueryID sensorID

        [ "nodes", _, "motors", motorID, "done-by-day", "last" ] ->
            Just <| LastWaterGivenQueryID motorID

        [ "nodes", _, "sensors", sensorID, "readings", "last" ] ->
            Just <| LastSensorReadingQueryID sensorID

        _ ->
            Nothing


queryIDToMessageDecoder : QueryID -> Json.Decode.Decoder Msg
queryIDToMessageDecoder queryID =
    case queryID of
        MotorsQueryID ->
            motorsQueryResponseDecoder

        SensorQueryID sensorID ->
            sensorQueryResponseDecoder sensorID

        LastWaterGivenQueryID motorID ->
            lastWaterGivenQueryResponseDecoder motorID

        LastSensorReadingQueryID sensorID ->
            lastSensorReadingQueryResponseDecoder sensorID


init : Model
init =
    { motors = Dict.empty
    , sensors = Dict.empty
    , lastWaterGivens = Dict.empty
    , lastSensorReadings = Dict.empty
    }



-- UPDATE


type Msg
    = MotorsReceived (List Motor)
    | SensorReceived String (Maybe Sensor)
    | LastWaterGivenReceived String (Maybe WaterGiven)
    | LastSensorReadingReceived String (Maybe SensorReading)


update : Msg -> Model -> ( Model, List Query.Query )
update msg model =
    case msg of
        MotorsReceived motors ->
            ( updateMotors motors model
            , queriesForMotors motors
            )

        SensorReceived sensorID maybeSensor ->
            ( updateSensor sensorID maybeSensor model
            , []
            )

        LastWaterGivenReceived motorID maybeWaterGiven ->
            ( updateWaterGiven motorID maybeWaterGiven model
            , []
            )

        LastSensorReadingReceived sensorID maybeSensorReading ->
            ( updateSensorReading sensorID maybeSensorReading model
            , []
            )


updateMotors : List Motor -> Model -> Model
updateMotors motorsList model =
    let
        motors =
            Dict.fromList <|
                List.map (\m -> ( m.id, m )) motorsList
    in
    { model | motors = motors }


updateSensor : String -> Maybe Sensor -> Model -> Model
updateSensor sensorID maybeSensor model =
    { model | sensors = Dict.update sensorID (\_ -> maybeSensor) model.sensors }


updateWaterGiven : String -> Maybe WaterGiven -> Model -> Model
updateWaterGiven motorID maybeWaterGiven model =
    { model | lastWaterGivens = Dict.update motorID (\_ -> maybeWaterGiven) model.lastWaterGivens }


updateSensorReading : String -> Maybe SensorReading -> Model -> Model
updateSensorReading sensorID maybeSensorReading model =
    { model | lastSensorReadings = Dict.update sensorID (\_ -> maybeSensorReading) model.lastSensorReadings }


modelToPlantInfos : Model -> List PlantInfo
modelToPlantInfos model =
    List.map (\motor -> motorToPlantInfo motor model) (Dict.values model.motors)


motorToPlantInfo motor model =
    { motor = motor
    , sensor = Dict.get motor.triggerSensorId model.sensors
    , lastWaterGiven = Dict.get motor.id model.lastWaterGivens
    , lastSensorReading = Dict.get motor.triggerSensorId model.lastSensorReadings
    }



-- VIEW


view : Time.Zone -> Time.Posix -> Model -> Element Msg
view zone time model =
    plantsView zone time (modelToPlantInfos model)


plantsView : Time.Zone -> Time.Posix -> List PlantInfo -> Element Msg
plantsView zone time plantInfos =
    Element.column [ padding 10, spacing 20, width fill, height fill ] <|
        listWithDefault emptyListView <|
            List.map (plantView zone time) plantInfos


emptyListView =
    Element.text "Nothing to show"


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