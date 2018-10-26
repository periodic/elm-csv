module Tests exposing (all, expectInvalid, expectParses)

import Csv exposing (Csv)
import Expect
import Parser exposing (Parser)
import Test exposing (..)


type alias CsvParser =
    String -> Result (List Parser.DeadEnd) Csv.Csv


expectParserParses : CsvParser -> String -> Csv.Csv -> Expect.Expectation
expectParserParses parser input expected =
    case parser input of
        Ok res ->
            if res == expected then
                Expect.pass

            else
                Expect.fail
                    ("Parse is incorrect.\n"
                        ++ "Expected: "
                        ++ csvToString expected
                        ++ "\n"
                        ++ "Actual: "
                        ++ csvToString res
                    )

        Err err ->
            Expect.fail ("Failed to parse input: \"" ++ input ++ "\"")


expectParses : String -> Csv.Csv -> Expect.Expectation
expectParses =
    expectParserParses Csv.parse


expectParsesWith : Char -> String -> Csv.Csv -> Expect.Expectation
expectParsesWith fieldSep =
    expectParserParses <| Csv.parseWith fieldSep


csvToString : Csv -> String
csvToString csv =
    String.concat <|
        [ String.concat (List.map (\hd -> "'" ++ hd ++ "'") csv.headers)
        , "\n"
        ]
            ++ List.map
                (\recs -> String.concat (List.map (\f -> "'" ++ f ++ "'") recs ++ [ "\n" ]))
                csv.records



-- ++ "\"\n" ++ toString err)


expectInvalid : String -> Expect.Expectation
expectInvalid input =
    case Csv.parse input of
        Ok res ->
            Expect.fail ("Expected input to fail, but it parsed successfully: " ++ input)

        Err _ ->
            Expect.pass


all : Test
all =
    describe "CSV Parser"
        [ describe "Value parsing"
            [ test "Empty input" <|
                \() ->
                    expectParses "" { headers = [ "" ], records = [] }
            , test "Simple values" <|
                \() ->
                    expectParses "a,1" { headers = [ "a", "1" ], records = [] }
            , test "Special characters" <|
                \() ->
                    expectParses "< £200,Allied\u{0092}s" { headers = [ "< £200", "Allied\u{0092}s" ], records = [] }
            , test "Empty value" <|
                \() ->
                    expectParses "a,,1" { headers = [ "a", "", "1" ], records = [] }
            , test "Preserves spaces" <|
                \() ->
                    expectParses "a ,  , 1" { headers = [ "a ", "  ", " 1" ], records = [] }
            , test "Quoted newlines" <|
                \() ->
                    expectParses "a,\"\nb\n\",c" { headers = [ "a", "\nb\n", "c" ], records = [] }
            , test "Quoted quotes" <|
                \() ->
                    expectParses "a,\"\"\"\",c" { headers = [ "a", "\"", "c" ], records = [] }
            , test "Quoted commas" <|
                \() ->
                    expectParses "a,\"b,b\",c" { headers = [ "a", "b,b", "c" ], records = [] }
            , test "Quotes with trailing spaces" <|
                \() ->
                    expectInvalid "\"a\" "
            , test "Quotes with leading spaces" <|
                \() ->
                    expectInvalid "  \"a\""
            ]
        , describe "Line terminators"
            [ test "NL only" <|
                \() ->
                    expectParses
                        "a,b,c\nd,e,f\ng,h,i\n"
                        { headers = [ "a", "b", "c" ], records = [ [ "d", "e", "f" ], [ "g", "h", "i" ] ] }
            , test "CR only 1" <|
                \() ->
                    expectParses
                        "a,b,c\u{000D}d,e,f\u{000D}g,h,i\u{000D}"
                        { headers = [ "a", "b", "c" ], records = [ [ "d", "e", "f" ], [ "g", "h", "i" ] ] }
            , test "CR only 2" <|
                \() ->
                    expectParses
                        "a,b,c\u{000D}\nd,e,f\u{000D}\ng,h,i\u{000D}\n"
                        { headers = [ "a", "b", "c" ], records = [ [ "d", "e", "f" ], [ "g", "h", "i" ] ] }
            , test "Mixed" <|
                \() ->
                    expectParses
                        "a,b,c\u{000D}d,e,f\ng,h,i\u{000D}\n"
                        { headers = [ "a", "b", "c" ], records = [ [ "d", "e", "f" ], [ "g", "h", "i" ] ] }
            ]
        , describe "Row parsing"
            [ test "Empty headers" <|
                \() ->
                    expectParses "\n" { headers = [ "" ], records = [] }
            , test "Empty headers, empty row" <|
                \() ->
                    expectParses "\n\n" { headers = [ "" ], records = [ [ "" ] ] }
            , test "Trailing newline" <|
                \() ->
                    expectParses "a\nb\n" { headers = [ "a" ], records = [ [ "b" ] ] }
            , test "Tabulated fields" <|
                \() ->
                    expectParsesWith '\t' "a\tb\naa\tbb" { headers = [ "a", "b" ], records = [ [ "aa", "bb" ] ] }
            , test "Tabulated fields 2" <|
                \() ->
                    expectParsesWith '\t' "a,b\naa,bb" { headers = [ "a,b" ], records = [ [ "aa,bb" ] ] }
            , test "Tabulated fields 3" <|
                \() ->
                    expectParsesWith ',' "a,b\naa,bb" { headers = [ "a", "b" ], records = [ [ "aa", "bb" ] ] }
            , test "Tabulated fields 4" <|
                \() ->
                    expectParsesWith '$' "a$b\naa$bb" { headers = [ "a", "b" ], records = [ [ "aa", "bb" ] ] }
            ]
        ]
