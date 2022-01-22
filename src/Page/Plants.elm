module Page.Plants exposing (Model, Msg, init, update, view)

import CollectionUtil exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Page.Plants.Plant as Plant
import Time



-- MODEL


type alias Model =
    { plants : Dict String Plant.Model }


init : Model
init =
    { plants = Dict.empty }



-- UPDATE


type Msg
    = PlantMsg String Plant.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlantMsg id plantMsg ->
            let
                ( plant, cmd ) =
                    Dict.get id model.plants
                        |> Maybe.withDefault Plant.init
                        |> Plant.update plantMsg
            in
            ( { model | plants = Dict.update id (\_ -> Just plant) model.plants }
            , cmd |> Cmd.map (PlantMsg id)
            )



-- VIEW


view : Time.Zone -> Time.Posix -> List Plant.PlantInfo -> Model -> Element Msg
view zone time plantInfos model =
    Element.column [ padding 10, spacing 20, width fill, height fill ] <|
        listWithDefault emptyListView <|
            List.map (plantView zone time model) plantInfos


plantView : Time.Zone -> Time.Posix -> Model -> Plant.PlantInfo -> Element Msg
plantView zone time model plantInfo =
    Dict.get plantInfo.motor.id model.plants
        |> Maybe.withDefault Plant.init
        |> Plant.view zone time plantInfo
        |> Element.map (PlantMsg plantInfo.motor.id)


emptyListView =
    Element.text "Nothing to show"
