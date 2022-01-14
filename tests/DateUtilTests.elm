module DateUtilTests exposing (..)

import DateUtil exposing (..)
import Expect
import Test exposing (..)
import Time


suite : Test
suite =
    describe "DateUtil module"
        [ describe "daysSince"
            -- 2 weeks ago
            [ test "start 2022-01-14 00:00:00-0800, current 2022-01-29 00:00:00-0800 - 14 days" <|
                let
                    zone =
                        Time.customZone (-8 * 60) []

                    startTime =
                        Time.millisToPosix 1642147200000

                    currentTime =
                        Time.millisToPosix 1643356800000
                in
                \_ -> Expect.equal 14 (daysSince zone currentTime startTime)
            ]
        , describe "humaneTimeSince"
            -- today
            [ test "start 2022-01-14 00:00:00-0800, current 2022-01-14 23:59:59-0800 - returns today" <|
                let
                    zone =
                        Time.customZone (-8 * 60) []

                    startTime =
                        Time.millisToPosix 1642147200000

                    currentTime =
                        Time.millisToPosix 1642233599000
                in
                \_ -> Expect.equal "today" (humaneTimeSince zone currentTime startTime)

            -- yesterday
            , test "start 2022-01-14 00:00:00-0800, current 2022-01-15 00:00:00-0800 - returns yesterday" <|
                let
                    zone =
                        Time.customZone (-8 * 60) []

                    startTime =
                        Time.millisToPosix 1642147200000

                    currentTime =
                        Time.millisToPosix 1642233600000
                in
                \_ -> Expect.equal "yesterday" (humaneTimeSince zone currentTime startTime)

            -- 2 days ago
            , test "start 2022-01-14 00:00:00-0800, current 2022-01-16 00:00:00-0800 - returns 2 days ago" <|
                let
                    zone =
                        Time.customZone (-8 * 60) []

                    startTime =
                        Time.millisToPosix 1642147200000

                    currentTime =
                        Time.millisToPosix 1642320000000
                in
                \_ -> Expect.equal "2 days ago" (humaneTimeSince zone currentTime startTime)

            -- 13 days ago
            , test "start 2022-01-14 00:00:00-0800, current 2022-01-28 23:59:59-0800 - returns 13 days ago" <|
                let
                    zone =
                        Time.customZone (-8 * 60) []

                    startTime =
                        Time.millisToPosix 1642147200000

                    currentTime =
                        Time.millisToPosix 1643356799000
                in
                \_ -> Expect.equal "13 days ago" (humaneTimeSince zone currentTime startTime)

            -- 2 weeks ago
            , test "start 2022-01-14 00:00:00-0800, current 2022-01-29 00:00:00-0800 - returns 2 weeks ago" <|
                let
                    zone =
                        Time.customZone (-8 * 60) []

                    startTime =
                        Time.millisToPosix 1642147200000

                    currentTime =
                        Time.millisToPosix 1643356800000
                in
                \_ -> Expect.equal "2 weeks ago" (humaneTimeSince zone currentTime startTime)
            ]
        , describe "durationConcise"
            [ test "less than a minutes displayed as seconds" <|
                \_ -> Expect.equal "59s" (durationConcise 59)
            , test "more than a minute displayed as minutes and seconds" <|
                \_ -> Expect.equal "1m 1s" (durationConcise 61)
            , test "seconds not shown for exact non zero minutes" <|
                \_ -> Expect.equal "1m" (durationConcise 60)
            ]
        ]
