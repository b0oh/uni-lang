module Uni.Term exposing (..)

type Literal
    = Int Int
    | Float Float
    | String String


type Term
    = Abs String Term
    | App Term Term
    | Literal Literal
    | Symbol String


make_abs_chain : List String -> Term -> Term
make_abs_chain binds body =
    let
        step sub_binds acc =
            case sub_binds of
                [] ->
                    acc

                bind :: rest ->
                    step rest (Abs bind acc)
    in
    step (List.reverse binds) body


to_string : Term -> String
to_string term =
    case term of
        Abs bind body ->
            String.concat
                [ "(\\"
                , bind
                , " -> "
                , to_string body
                , ")"
                ]

        App abs arg ->
            String.concat
                [ "("
                , to_string abs
                , " "
                , to_string arg
                , ")"
                ]

        Symbol sym ->
            sym

        Literal (Int int) ->
            "(lit " ++ String.fromInt int ++ ")"

        Literal (Float float) ->
            "(lit " ++ String.fromFloat float ++ ")"

        Literal (String string) ->
            "(lit \"" ++ string ++ "\")"
