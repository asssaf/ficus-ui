port module Log exposing (Model, Msg(..), init, subscriptions, update, view)

import Element exposing (Element)
import Json.Decode exposing (..)
import Result.Extra as Result



-- PORTS


port logReceiver : (Json.Decode.Value -> msg) -> Sub msg



-- MODEL


type Model
    = NoError
    | Error String


type alias LogItem =
    { message : String
    , level : String
    }


init : Model
init =
    NoError



-- UPDATE


type Msg
    = ErrorOccurred String
    | ErrorCleared
    | LogItemReceived Json.Decode.Value
    | LogItemAdded LogItem


update : Msg -> Model -> Model
update msg =
    case msg of
        ErrorOccurred err ->
            \_ -> Error err

        ErrorCleared ->
            \_ -> NoError

        LogItemReceived logItem ->
            update (decodeLogItemAndExtract logItem)

        LogItemAdded logItem ->
            case logItem.level of
                "error" ->
                    update (ErrorOccurred logItem.message)

                _ ->
                    update ErrorCleared


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


parseErrorToMessage : Json.Decode.Error -> Msg
parseErrorToMessage err =
    ErrorOccurred (Json.Decode.errorToString err)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    logReceiver LogItemReceived



-- VIEW


view : Model -> Element Msg
view model =
    case model of
        NoError ->
            Element.none

        Error err ->
            Element.paragraph []
                [ Element.text err ]
