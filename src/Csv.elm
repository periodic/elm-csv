module Csv exposing (Csv, parse, parseWith)

{-| A parser for transforming CSV strings into usable input.

This library does its best to support RFC 4180, however, many CSV inputs do not strictly follow the standard.  There are two major deviations assumed in this library.

1. The `\n` or `\r` character may be used instead of `\r\n` for line separators.
2. The trailing new-line may be omitted.

RFC 4180 grammar, for reference, with notes.

The trailing newline is required, but we'll make it optional.

    file = [header CRLF] record *(CRLF record) [CRLF]
    header = name *(COMMA name)
    record = field *(COMMA field)
    name = field
    field = (escaped / non-escaped)

There is no room for spaces around the quotes.  The specification is that

    escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE

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
@docs parseWith
-}

import String
import Result
import Combine exposing (Parser, (<*), (*>), (<*>), (<$), ($>), (<$>), (<|>), (<?>))
import Combine.Char exposing (char, noneOf)


{-| Represents a CSV document.  All CSV documents are have a header row, even if that row is empty.
-}
type alias Csv =
    { headers : List String
    , records : List (List String)
    }

type alias Config =
    { fieldSep : Char
    }

{-| Parse a CSV string into it's constituent fields using COMMA as a field separator.
-}
parse : String -> Result (List String) Csv
parse = parseWith ','

{-| Parse a CSV string into it's constituent fields with a configurable field separator.
-}
parseWith : Char -> String -> Result (List String) Csv
parseWith fieldSep =
    let
        config = { fieldSep = fieldSep }
    in
        addTrailingLineSep >> Combine.parse (file config) >> Result.mapError thrd >> Result.map thrd

{-| Gets the third element of a tuple.
-}
thrd : ( a, b, c ) -> c
thrd ( _, _, c ) =
    c


{-| Adds a trailing line separator to a string if not present.
-}
addTrailingLineSep : String -> String
addTrailingLineSep str =
    if not (String.endsWith "\n" str || String.endsWith "\x0D" str) then
        str ++ "\x0D\n"
    else
        str


doubleQuote : Parser s Char
doubleQuote =
    char '"'


cr : Parser s Char
cr =
    char '\x0D'


lf : Parser s Char
lf =
    char '\n'



-- This should probably return \r\n, but I'm never going to actually use it.


lineSep : Parser s ()
lineSep =
    -- Prefer the multi-character code, but accept others.
    ()
        <$ ((cr *> lf) <|> cr <|> lf)
        <?> "Expected new line."


doubleDoubleQuote : Parser s Char
doubleDoubleQuote =
    (doubleQuote *> doubleQuote)



-- Grab all non-quote data.


textData : Config -> Parser s Char
textData config =
    noneOf [ '"', config.fieldSep , '\n', '\x0D' ]


nonEscaped : Config -> Parser s String
nonEscaped config =
    Combine.many (textData config)
        |> Combine.map String.fromList
        |> Combine.mapError (always [ "Expected non-escaped value." ])


escaped : Config -> Parser s String
escaped config =
    let
        innerChar =
            Combine.choice [ textData config, char config.fieldSep, cr, lf, doubleDoubleQuote ]

        innerString =
            Combine.many innerChar |> Combine.map String.fromList
    in
        (doubleQuote *> innerString <* doubleQuote)
            |> Combine.mapError (always [ "Expected escaped value." ])


field : Config -> Parser s String
field config =
    Combine.choice [ escaped config, nonEscaped config ]


name : Config -> Parser s String
name =
    field


header : Config -> Parser s (List String)
header = record


record : Config -> Parser s (List String)
record config =
    Combine.sepBy (char config.fieldSep) (field config)


file : Config -> Parser s Csv
file config =
    Csv
        <$> header config
        <* (lineSep <?> "Unterminated header")
        <*> Combine.many (record config <* (lineSep <?> "Unterminated record"))
        <* Combine.end
