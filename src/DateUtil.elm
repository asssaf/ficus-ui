module DateUtil exposing (..)

import Date exposing (Date)
import Time


daysSince : Time.Zone -> Time.Posix -> Time.Posix -> Int
daysSince zone currentTime olderTime =
    Date.diff Date.Days
        (Date.fromPosix zone olderTime)
        (Date.fromPosix zone currentTime)


daysSinceHumane : Time.Zone -> Time.Posix -> Time.Posix -> String
daysSinceHumane zone currentTime olderTime =
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
