module Sexp exposing (Sexp(..), fromString)

type Sexp
    = Symbol String
    | List (List Sexp)


type alias State =
    { result : Sexp, leftover : String }


closeParen =
    Char.fromCode 41


openParen =
    Char.fromCode 40


isWhite : Char -> Bool
isWhite char =
    List.member char (String.toList " \n")


isSymbolDelimiter : Char -> Bool
isSymbolDelimiter char =
    isWhite char || char == openParen || char == closeParen


isSymbolChar char =
    not (isSymbolDelimiter char)


readSpacedSymbol : String -> String -> Result String State
readSpacedSymbol input acc =
    case String.uncons input of
        Nothing ->
            Err "expected to see \""

        Just ( char, rest ) ->
            if char == '"' then
                Ok { result = Symbol (String.reverse acc), leftover = rest }

            else
                String.cons char acc
                    |> readSpacedSymbol rest


readSymbol : String -> String -> State
readSymbol input acc =
    case String.uncons input of
        Nothing ->
            { result = Symbol (String.reverse acc)
            , leftover = ""
            }

        Just ( char, rest ) ->
            if isSymbolDelimiter char then
                { result = Symbol (String.reverse acc)
                , leftover = input
                }

            else
                String.cons char acc
                    |> readSymbol rest


readList : String -> List Sexp -> Result String State
readList input acc =
    case String.uncons input of
        Nothing ->
            Err "unexpected end of input"

        Just ( char, rest ) ->
            if isWhite char then
                readList rest acc

            else if char == closeParen then
                Ok { result = List (List.reverse acc), leftover = rest }

            else
                case readAny input of
                    Err reason ->
                        Err reason

                    Ok { result, leftover } ->
                        readList leftover (result :: acc)


readAny : String -> Result String State
readAny input =
    case String.uncons input of
        Nothing ->
            Err "unexpected end of input"

        Just ( char, rest ) ->
            if isWhite char then
                readAny rest

            else if char == openParen then
                readList rest []

            else if char == '"' then
                readSpacedSymbol rest ""

            else if isSymbolChar char then
                String.fromChar char
                    |> readSymbol rest
                    |> Ok

            else
                Err ("unexpected symbol: " ++ String.fromChar char)


fromString : String -> Result String Sexp
fromString input =
    case readAny input of
        Err reason ->
            Err reason

        Ok { result } ->
            Ok result
