module Page.Plants.Plant exposing (Model, Msg, PlantInfo, init, update, view)

import CollectionUtil exposing (..)
import Constants exposing (..)
import DateUtil exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import ExpandableList exposing (expandableList)
import Motor exposing (Motor)
import Sensor exposing (Sensor)
import SensorReading exposing (SensorReading)
import Time
import WaterGiven exposing (WaterGiven)



-- MODEL


type alias Model =
    { waterGivensExpanded : Bool }


type alias PlantInfo =
    { motor : Motor
    , sensor : Maybe Sensor
    , latestWaterGivens : List WaterGiven
    , lastSensorReading : Maybe SensorReading
    }


init : Model
init =
    { waterGivensExpanded = False }



-- UPDATE


type Msg
    = WaterGivenToggled


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WaterGivenToggled ->
            ( { model | waterGivensExpanded = not model.waterGivensExpanded }
            , Cmd.none
            )



-- VIEW


view : Time.Zone -> Time.Posix -> PlantInfo -> Model -> Element Msg
view zone time plantInfo model =
    Element.column
        [ Background.color tileBGColor
        , Font.color (rgb255 0 0 0)
        , Border.rounded 10
        , padding 20
        , spacing 10
        , width (fill |> minimum 250 |> maximum 800)
        , centerX
        ]
        [ plantViewHeader plantInfo model
        , plantViewBody zone time plantInfo model
        ]


tileBGColor =
    lightBlue


plantViewHeader : PlantInfo -> Model -> Element Msg
plantViewHeader plantInfo _ =
    Element.el [ Font.bold ] (Element.text (plantInfo.motor.name ++ " | " ++ plantInfo.motor.nodeID))


plantViewBody : Time.Zone -> Time.Posix -> PlantInfo -> Model -> Element Msg
plantViewBody zone time plantInfo model =
    Element.row [ spacing 30, width fill ]
        [ Element.el [ width (fillPortion 7), height fill ] <| plantViewBodyLeft zone time plantInfo model
        , Element.el [ width (fillPortion 3), height fill ] <| plantViewBodyRight zone time plantInfo model
        ]


plantViewBodyLeft : Time.Zone -> Time.Posix -> PlantInfo -> Model -> Element Msg
plantViewBodyLeft zone time plantInfo model =
    Element.column [ spacing 5 ]
        [ motorLastWateredLabel zone time plantInfo.latestWaterGivens model
        , lastReadingLabel zone time plantInfo.sensor plantInfo.lastSensorReading
        ]


plantViewBodyRight : Time.Zone -> Time.Posix -> PlantInfo -> Model -> Element Msg
plantViewBodyRight _ _ plantInfo _ =
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


motorLastWateredLabel : Time.Zone -> Time.Posix -> List WaterGiven -> Model -> Element Msg
motorLastWateredLabel zone time waterGivens model =
    Element.row []
        [ Element.el [ alignTop ] (Element.text "Last watered: ")
        , expandableList WaterGivenToggled model.waterGivensExpanded <|
            listWithDefault (Element.text "Never") <|
                List.map (\wg -> Element.paragraph [] <| [ Element.text (formatWaterGiven zone time wg) ]) waterGivens
        ]


formatWaterGiven : Time.Zone -> Time.Posix -> WaterGiven -> String
formatWaterGiven zone time waterGiven =
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
