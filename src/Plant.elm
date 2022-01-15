module Plant exposing (Model, init, updateMotors, updateSensor, updateSensorReading, updateWaterGiven)

import Dict exposing (Dict)
import Motor exposing (Motor)
import Sensor exposing (Sensor)
import SensorReading exposing (SensorReading)
import WaterGiven exposing (WaterGiven)


type alias Model =
    { motors : Dict String Motor
    , sensors : Dict String Sensor
    , lastWaterGivens : Dict String WaterGiven
    , lastSensorReadings : Dict String SensorReading
    }


init : Model
init =
    { motors = Dict.empty
    , sensors = Dict.empty
    , lastWaterGivens = Dict.empty
    , lastSensorReadings = Dict.empty
    }


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
