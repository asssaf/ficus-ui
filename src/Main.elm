port module Main exposing (..)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Element exposing (..)
import Header
import Json.Decode
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
    { user : Maybe UserInfo
    , width : Int
    , height : Int
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , user : User
    , screen : ScreenModel
    , zone : Time.Zone
    , time : Time.Posix
    , log : Log.Model
    , plants : Plant.Model
    }


type alias ScreenModel =
    { width : Int
    , height : Int
    }


type User
    = SignedOut
    | SignedIn UserInfo
    | SigningOut


type alias UserInfo =
    { uid : String
    }


type QueryID
    = PlantQueryID Plant.QueryID


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , user = Maybe.withDefault SignedOut <| Maybe.map SignedIn flags.user
      , screen = { width = flags.width, height = flags.height }
      , zone = Time.utc
      , time = Time.millisToPosix 0
      , log = Log.init
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
            Cmd.batch <|
                List.concat
                    [ [ Task.perform AdjustTimeZone Time.here
                      , Task.perform Tick Time.now
                      ]
                    , List.map sendQuery Plant.initQueries
                    ]



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | Resized Int Int
    | QueryResponseReceived Json.Decode.Value
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

        HeaderMsg headerMsg ->
            case headerMsg of
                Header.SignOut ->
                    ( { model | user = SigningOut }
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
            [ Plant.decodeQueryID >> Maybe.map PlantQueryID ]
    in
    List.filterMap (\dec -> dec segments) decoders
        |> List.head


queryIDToMessageDecoder : QueryID -> Json.Decode.Decoder Msg
queryIDToMessageDecoder queryID =
    case queryID of
        PlantQueryID plantQueryID ->
            Plant.queryIDToMessageDecoder plantQueryID
                |> Json.Decode.map PlantMsg



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
        SignedOut ->
            loginView model

        SignedIn user ->
            mainView user model

        SigningOut ->
            signingOutView model


loginView : Model -> Browser.Document Msg
loginView model =
    { title = "Ficus"
    , body =
        [ Element.layout [ width (px model.screen.width), height (px model.screen.height) ] <|
            Element.map LoginMsg <|
                Login.view
        ]
    }


signingOutView : Model -> Browser.Document Msg
signingOutView model =
    { title = "Ficus"
    , body =
        [ Element.layout [ width (px model.screen.width), height (px model.screen.height) ] <|
            Element.el [ width fill, height fill ] <|
                Element.el [ centerX, centerY ] (Element.text "Signing out...")
        ]
    }


mainView : UserInfo -> Model -> Browser.Document Msg
mainView _ model =
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
