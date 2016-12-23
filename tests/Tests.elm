module Tests exposing (..)

import Test exposing (..)
import Expect
import Csv

expectParses : String -> Csv.Csv -> Expect.Expectation
expectParses input expected =
    case Csv.parse input of
        Ok res ->
            if res == expected
                then Expect.pass
                else
                    Expect.fail ("Parse is incorrect.\n" ++
                                 "Expected: " ++ toString expected ++ "\n" ++
                                 "Actual: " ++ toString res)
        Err err ->
            Expect.fail ("Failed to parse input: \"" ++ toString input ++ "\"\n" ++ toString err)

all : Test
all =
    describe "CSV Parser"
        [ describe "Value parsing"
            [ test "Empty input" <|
                \() ->
                    expectParses "" { headers = [""], records = [] }
            , test "Simple values" <|
                \() ->
                    expectParses "a,1" { headers = ["a", "1"], records = [] }
            , test "Empty value" <|
                \() ->
                    expectParses "a,,1" { headers = ["a", "", "1"], records = [] }
            , test "Preserves spaces" <|
                \() ->
                    expectParses "a ,  , 1" { headers = ["a ", "  ", " 1"], records = [] }
            , test "Quoted newlines" <|
                \() ->
                    expectParses "a,\"\nb\n\",c" { headers = ["a", "\nb\n", "c"], records = [] }
            , test "Quoted quotes" <|
                \() ->
                    expectParses "a,\"\"\"\",c" { headers = ["a", "\"", "c"], records = [] }
            , test "Quoted commas" <|
                \() ->
                    expectParses "a,\"b,b\",c" { headers = ["a", "b,b", "c"], records = [] }
            ]
        , describe "Row parsing"
            [ test "Multi-row input" <|
                \() ->
                    expectParses "a,b,c\r\nd,e,f\r\ng,h,i" { headers = ["a", "b", "c"], records = [["d", "e", "f"], ["g", "h", "i"]] }
            , test "Empty headers" <|
                \() ->
                    expectParses "\n" { headers = [""], records = [] }
            , test "Empty headers, empty row" <|
                \() ->
                    expectParses "\n\n" { headers = [""], records = [[""]] }
            , test "Trailing newline" <|
                \() ->
                    expectParses "a\nb\n" { headers = ["a"], records = [["b"]] }
            ]
        {- Currently doesn't work.  Tends to run indefinitely.
        , describe "Catch-all fuzz test"
            [ fuzz (list (list string)) "Random escaped input parses back to the input." <|
                \records ->
                    let
                        quoteRegex = Regex.regex "\""
                        commaRegex = Regex.regex ","
                        escapeValue val =
                            if Regex.contains quoteRegex val || Regex.contains commaRegex val
                                then "\"" ++ (Regex.replace Regex.All quoteRegex (\_ -> "\"\"") val) ++ "\""
                                else val
                        csv =
                            records
                                |> List.map (String.join "," << List.map escapeValue)
                                |> String.join "\r\n"
                                |> flip (++) "\r\n"
                        expected =
                            { headers = Maybe.withDefault [] <| List.head records
                            , records = Maybe.withDefault [] <| List.tail records
                            }
                    in
                        expectParses csv expected
            ]
        -}
        ]
