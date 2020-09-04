module Sexp exposing (Sexp(..), from_string)

import Base exposing (..)
import Base.Try as Try
import Base.String as String


type Sexp
    = Symbol String
    | List (List Sexp)


type alias State =
    { result : Sexp
    , leftover : String
    }


close_paren =
    Char.fromCode 41


open_paren =
    Char.fromCode 40


is_white : Char -> Bool
is_white char =
    List.member char (String.to_list " \n")


is_symbol_delimiter : Char -> Bool
is_symbol_delimiter char =
    is_white char || char == open_paren || char == close_paren


is_symbol_char char =
    not (is_symbol_delimiter char)


read_quoted_symbol : String -> String -> Try String State
read_quoted_symbol input acc =
    case String.uncons input of
        Nothing ->
            Try.fail "expected to see \""

        Just ( char, rest ) ->
            if char == '"' then
                Try.succeed
                    { result = Symbol (String.reverse acc)
                    , leftover = rest
                    }

            else
                String.cons char acc
                    |> read_quoted_symbol rest


read_symbol : String -> String -> State
read_symbol input acc =
    case String.uncons input of
        Nothing ->
            { result = Symbol (String.reverse acc)
            , leftover = ""
            }

        Just ( char, rest ) ->
            if is_symbol_delimiter char then
                { result = Symbol (String.reverse acc)
                , leftover = input
                }

            else
                String.cons char acc
                    |> read_symbol rest


read_list : String -> List Sexp -> Try String State
read_list input acc =
    case String.uncons input of
        Nothing ->
            Try.fail "unexpected end of input"

        Just ( char, rest ) ->
            if is_white char then
                read_list rest acc

            else if char == close_paren then
                Try.succeed
                    { result = List (List.reverse acc)
                    , leftover = rest
                    }

            else
                Try.do (read_any input) <| \{ result, leftover } ->
                read_list leftover (result :: acc)


read_any : String -> Try String State
read_any input =
    case String.uncons input of
        Nothing ->
            Try.fail "unexpected end of input"

        Just ( char, rest ) ->
            if is_white char then
                read_any rest

            else if char == open_paren then
                read_list rest []

            else if char == '"' then
                read_quoted_symbol rest ""

            else if is_symbol_char char then
                String.from_char char
                    |> read_symbol rest
                    |> Try.succeed

            else
                Try.fail ("unexpected symbol: " ++ String.from_char char)


from_string : String -> Try String Sexp
from_string input =
    read_any input
        |> Try.map .result
