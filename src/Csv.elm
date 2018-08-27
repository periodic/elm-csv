module Csv exposing (..)

{-| A parser for transforming CSV strings into usable input.

This library does its best to support RFC 4180, however, many CSV inputs do not strictly follow the standard. There are two major deviations assumed in this library.

1.  The `\n` or `\r` character may be used instead of `\r\n` for line separators.
2.  The trailing new-line may be omitted.

RFC 4180 grammar, for reference, with notes.

The trailing newline is required, but we'll make it optional.

    file =
        [ header CRLF ] record * CRLF record [ CRLF ]

    header =
        name * COMMA name

    record =
        field * COMMA field

    name =
        field

    field =
        escaped / non - escaped

There is no room for spaces around the quotes. The specification is that

    escaped =
        DQUOTE * (TEXTDATA / COMMA / CR / LF / 2 DQUOTE) DQUOTE

In this specification, fields that don't have quotes surrounding them cannot have a quote inside them because it is excluded from `TEXTDATA`.

    non-escaped = *TEXTDATA
    COMMA = %x2C
    CR = %x0D ;as per section 6.1 of RFC 2234 [2]
    DQUOTE =  %x22 ;as per section 6.1 of RFC 2234 [2]
    LF = %x0A ;as per section 6.1 of RFC 2234 [2]

The spec requires that new lines be `CR + LF` but we'll let them get away with just `LF` if they want..

    CRLF = CR LF ;as per section 6.1 of RFC 2234 [2]

All the printable characters minus the double-quote and comma, this is important above.

    TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E


# Types

@docs Csv


# Functions

@docs parse

-}

-- exposing (Csv, parse)

import Hex
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , andThen
        , chompIf
        , chompWhile
        , float
        , getChompedString
        , keyword
        , lazy
        , loop
          --       , keep
        , oneOf
          --        , oneOrMore
          --       , repeat
        , run
          --      , source
        , succeed
        , symbol
          --     , zeroOrMore
        )
import Result
import String


-- import Combine exposing (Parser, (<*), (*>), (<*>), (<$), ($>), (<$>), (<|>), (<?>))
-- import Combine.Char exposing (char, noneOf)


{-| Represents a CSV document. All CSV documents are have a header row, even if that row is empty.
-}
type alias Csv =
    { headers : List String
    , records : List (List String)
    }



{- | Parse a CSV string into it's constituent fields. -}


parse : String -> Result (List Parser.DeadEnd) Csv
parse =
    addTrailingLineSep
        >> Parser.run file


{-| Gets the third element of a tuple.
-}
thrd : ( a, b, c ) -> c
thrd ( _, _, c ) =
    c



-- crs = "\u{000d}"


crs =
    "placeholder"



-- crc = '\u{000D}'


crc =
    'p'


{-| Adds a trailing line separator to a string if not present.
-}
addTrailingLineSep : String -> String
addTrailingLineSep str =
    if not (String.endsWith "\n" str || String.endsWith crs str) then
        str ++ crs ++ "\n"
    else
        str


comma : Parser ()
comma =
    symbol ","


doubleQuote : Parser ()
doubleQuote =
    symbol "\""


cr : Parser ()
cr =
    symbol crs


lf : Parser ()
lf =
    symbol "\n"


lineSep : Parser ()
lineSep =
    -- Prefer the multi-character code, but accept others.
    oneOf
        [ cr
        , cr |. lf
        , lf
        ]


doubleDoubleQuote : Parser ()
doubleDoubleQuote =
    doubleQuote |. doubleQuote



-- Grab all non-quote data.


textData : Parser ()
textData =
    chompIf (\c -> not (List.member c [ '"', ',', '\n', crc ]))


textChar : Char -> Bool
textChar c =
    not (List.member c [ '"', ',', '\n', crc ])


nonEscaped : Parser String
nonEscaped =
    getChompedString (chompWhile textChar)


innerChar : Parser String
innerChar =
    getChompedString
        (oneOf [ textData, comma, cr, lf, doubleDoubleQuote ])


innerString : List String -> Parser (Step (List String) String)
innerString strs =
    oneOf
        [ succeed (\str -> Loop (str :: strs)) |= innerChar
        , succeed ()
            |> Parser.map (\_ -> Done (String.concat (List.reverse strs)))
        ]


escaped : Parser String
escaped =
    succeed identity
        |. doubleQuote
        |= loop [] innerString
        |. doubleQuote


field : Parser String
field =
    oneOf [ escaped, nonEscaped ]


name : Parser String
name =
    field


recordHelper : List String -> Parser (Step (List String) (List String))
recordHelper strs =
    oneOf
        [ succeed (\str -> Loop (str :: strs))
            |= field
            |. comma
        , succeed (\str -> Done (List.reverse (str :: strs)))
            |= field
        ]


record : Parser (List String)
record =
    loop [] recordHelper


recordsHelper : List (List String) -> Parser (Step (List (List String)) (List (List String)))
recordsHelper records =
    oneOf
        [ succeed (\rec -> Loop (rec :: records))
            |= record
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse records))
        ]


file : Parser Csv
file =
    succeed Csv
        |= record
        |= loop [] recordsHelper
