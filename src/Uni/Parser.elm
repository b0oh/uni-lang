module Uni.Parser exposing (parse)

import Base exposing (..)
import Parser exposing ((|.), (|=), Parser)
import Uni.Term as Term exposing (Literal, Term)


do =
    flip Parser.andThen


string : Parser String
string =
    let
        error_message =
            "I reached the end of the input while trying to parse a quoted string."

        step : List String -> Parser (Parser.Step (List String) (Result String String))
        step reversed_chunks =
            let
                error _ =
                    error_message
                        |> Err
                        |> Parser.Done

                done _ =
                    reversed_chunks
                        |> List.reverse
                        |> String.concat
                        |> Ok
                        |> Parser.Done
            in
            Parser.oneOf
                [ Parser.map error Parser.end
                , Parser.token "\""
                    |> Parser.map done
                , Parser.chompWhile (\c -> c /= '"')
                    |> Parser.getChompedString
                    |> Parser.map (\chunk -> Parser.Loop (chunk :: reversed_chunks))
                ]
    in
    do (Parser.token "\"") <| \_ ->
    do (Parser.loop [] step) <| \result ->
    case result of
        Ok string_ ->
            Parser.succeed string_

        Err err ->
            Parser.problem err


literal : Parser Term
literal =
    let
        number =
            Parser.oneOf
                [ Parser.map Term.Int Parser.int |> Parser.backtrackable
                , Parser.map Term.Float Parser.float
                ]
    in
    Parser.oneOf
        [ number
        , Parser.map Term.String string
        ]
        |> Parser.map Term.Literal


is_symbol_delimiter char =
    char == '\n' || char == '\t' || char == ' ' || char == '(' || char == ')'


symbol =
    Parser.chompWhile (is_symbol_delimiter >> not)
        |> Parser.getChompedString
        |> Parser.map Term.Symbol


application =
    do (Parser.token "(") <| \_ ->
    do term <| \abs ->
    do term <| \arg ->
    do Parser.spaces <| \_ ->
    do (Parser.token ")") <| \_ ->
    Parser.succeed (Term.App abs [arg])


term : Parser Term
term =
    do Parser.spaces <| \_ ->
    Parser.oneOf
        [ application
        , literal
        , symbol
        ]


parser : Parser (List Term)
parser =
    let
        step : List Term -> Parser (Parser.Step (List Term) (List Term))
        step reversed_terms =
            let
                loop term_ =
                    Parser.Loop (term_ :: reversed_terms)

                done _ =
                    Parser.Done (List.reverse reversed_terms)
            in
            Parser.oneOf
                [ Parser.end
                    |> Parser.map done
                , Parser.succeed loop
                    |= term
                    |. Parser.spaces
                ]
    in
    Parser.loop [] step


parse : String -> Try String (List Term)
parse input =
    case Parser.run parser input of
        Ok value ->
            Success value

        Err error ->
            Failure "error"
