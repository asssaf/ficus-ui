module DateUtil exposing (..)

import Date
import Json.Decode
import Time


daysSince : Time.Zone -> Time.Posix -> Time.Posix -> Int
daysSince zone currentTime olderTime =
    Date.diff Date.Days
        (Date.fromPosix zone olderTime)
        (Date.fromPosix zone currentTime)


humaneTimeSince : Time.Zone -> Time.Posix -> Time.Posix -> String
humaneTimeSince zone currentTime olderTime =
    let
        days =
            daysSince zone currentTime olderTime
    in
    if days == 0 then
        "today"

    else if days == 1 then
        "yesterday"

    else if days < 14 then
        String.fromInt days ++ " days ago"

    else
        String.fromInt (days // 7) ++ " weeks ago"


durationConcise : Int -> String
durationConcise seconds =
    if seconds < 60 then
        String.fromInt seconds ++ "s"

    else
        let
            minutes =
                seconds // 60

            remainingSeconds =
                seconds - (minutes * 60)

            minutesPart =
                String.fromInt minutes ++ "m"
        in
        if remainingSeconds == 0 then
            minutesPart

        else
            minutesPart ++ " " ++ durationConcise remainingSeconds


posixDecoder : Json.Decode.Decoder Time.Posix
posixDecoder =
    Json.Decode.int
        |> Json.Decode.map (\s -> Time.millisToPosix (s * 1000))
