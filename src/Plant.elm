module Plant exposing
    ( Model
    , Msg
    , QueryID
    , decodeQueryID
    , init
    , initQueries
    , modelToPlants
    , queryIDToMessageDecoder
    , update
    )

import Dict exposing (Dict)
import Json.Decode
import Motor exposing (Motor, motorDecoder)
import Node exposing (Node, nodeDecoder)
import Page.Plants.Plant as Plant
import Query
import Sensor exposing (Sensor, sensorDecoder)
import SensorReading exposing (SensorReading, sensorReadingDecoder)
import WaterGiven exposing (WaterGiven, waterGivenDecoder)



-- MODEL


type alias Model =
    { nodes : Dict String Node
    , motors : Dict String Motor
    , sensors : Dict String Sensor
    , latestWaterGivens : Dict String (List WaterGiven)
    , lastSensorReadings : Dict String SensorReading
    }


type QueryID
    = NodesQueryID
    | MotorsQueryID
    | SensorQueryID String
    | LatestWaterGivensQueryID String
    | LastSensorReadingQueryID String


nodesQuery : Query.Query
nodesQuery =
    { id = Query.segmentsToQueryID [ "nodes" ]
    , path = [ "nodes" ]
    , whereElements = []
    , orderBy = Nothing
    , limit = Just 10
    , collectionGroup = False
    }


nodesQueryResponseDecoder : Json.Decode.Decoder Msg
nodesQueryResponseDecoder =
    nodeDecoder
        |> Json.Decode.field "data"
        |> Json.Decode.list
        |> Json.Decode.field "docs"
        |> Json.Decode.map NodesReceived


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
    [ latestWaterGivensQuery motor.nodeID motor.id
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


latestWaterGivensQuery : String -> String -> Query.Query
latestWaterGivensQuery nodeID motorID =
    { id = Query.segmentsToQueryID [ "nodes", nodeID, "motors", motorID, "done-by-day", "latest" ]
    , path = [ "nodes", nodeID, "motors", motorID, "done-by-day" ]
    , whereElements = []
    , orderBy = Just { field = "start", dir = "desc" }
    , limit = Just 3
    , collectionGroup = False
    }


latestWaterGivensQueryResponseDecoder : String -> Json.Decode.Decoder Msg
latestWaterGivensQueryResponseDecoder motorID =
    waterGivenDecoder
        |> Json.Decode.field "data"
        |> Json.Decode.list
        |> Json.Decode.field "docs"
        |> Json.Decode.map (LatestWaterGivensReceived motorID)


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
        [ "nodes" ] ->
            Just NodesQueryID

        [ "motors" ] ->
            Just MotorsQueryID

        [ "nodes", _, "sensors", sensorID ] ->
            Just <| SensorQueryID sensorID

        [ "nodes", _, "motors", motorID, "done-by-day", "latest" ] ->
            Just <| LatestWaterGivensQueryID motorID

        [ "nodes", _, "sensors", sensorID, "readings", "last" ] ->
            Just <| LastSensorReadingQueryID sensorID

        _ ->
            Nothing


queryIDToMessageDecoder : QueryID -> Json.Decode.Decoder Msg
queryIDToMessageDecoder queryID =
    case queryID of
        NodesQueryID ->
            nodesQueryResponseDecoder

        MotorsQueryID ->
            motorsQueryResponseDecoder

        SensorQueryID sensorID ->
            sensorQueryResponseDecoder sensorID

        LatestWaterGivensQueryID motorID ->
            latestWaterGivensQueryResponseDecoder motorID

        LastSensorReadingQueryID sensorID ->
            lastSensorReadingQueryResponseDecoder sensorID


init : Model
init =
    { nodes = Dict.empty
    , motors = Dict.empty
    , sensors = Dict.empty
    , latestWaterGivens = Dict.empty
    , lastSensorReadings = Dict.empty
    }


initQueries : List Query.Query
initQueries =
    [ nodesQuery
    , motorsQuery
    ]



-- UPDATE


type Msg
    = NodesReceived (List Node)
    | MotorsReceived (List Motor)
    | SensorReceived String (Maybe Sensor)
    | LatestWaterGivensReceived String (List WaterGiven)
    | LastSensorReadingReceived String (Maybe SensorReading)


update : Msg -> Model -> ( Model, List Query.Query )
update msg model =
    case msg of
        NodesReceived nodes ->
            ( updateNodes nodes model
            , []
            )

        MotorsReceived motors ->
            ( updateMotors motors model
            , queriesForMotors motors
            )

        SensorReceived sensorID maybeSensor ->
            ( updateSensor sensorID maybeSensor model
            , []
            )

        LatestWaterGivensReceived motorID waterGivens ->
            ( updateWaterGivens motorID waterGivens model
            , []
            )

        LastSensorReadingReceived sensorID maybeSensorReading ->
            ( updateSensorReading sensorID maybeSensorReading model
            , []
            )


updateNodes : List Node -> Model -> Model
updateNodes nodesList model =
    let
        nodes =
            Dict.fromList <|
                List.map (\n -> ( n.id, n )) nodesList
    in
    { model | nodes = nodes }


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


updateWaterGivens : String -> List WaterGiven -> Model -> Model
updateWaterGivens motorID waterGivens model =
    { model | latestWaterGivens = Dict.update motorID (\_ -> Just waterGivens) model.latestWaterGivens }


updateSensorReading : String -> Maybe SensorReading -> Model -> Model
updateSensorReading sensorID maybeSensorReading model =
    { model | lastSensorReadings = Dict.update sensorID (\_ -> maybeSensorReading) model.lastSensorReadings }


modelToPlants : Model -> List Plant.PlantInfo
modelToPlants model =
    List.map (\motor -> motorToPlant motor model) (Dict.values model.motors)


motorToPlant : Motor -> Model -> Plant.PlantInfo
motorToPlant motor model =
    { motor = motor
    , sensor = Dict.get motor.triggerSensorId model.sensors
    , latestWaterGivens = Maybe.withDefault [] <| Dict.get motor.id model.latestWaterGivens
    , lastSensorReading = Dict.get motor.triggerSensorId model.lastSensorReadings
    }
