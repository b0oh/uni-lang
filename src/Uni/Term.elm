module Uni.Term exposing (..)


type Literal
    = Int Int
    | Float Float
    | String String


type Term
    = Abs (List String) Term
    | App Term (List Term)
    | Literal Literal
    | Symbol String


to_string : Term -> String
to_string term =
    case term of
        Abs syms body ->
            String.concat
                [ "(\\"
                , syms
                    |> String.join " "
                , " -> "
                , to_string body
                , ")"
                ]

        App abs args ->
            String.concat
                [ "("
                , to_string abs
                , " "
                , args
                    |> List.map to_string
                    |> String.join " "
                , ")"
                ]

        Symbol sym ->
            sym

        Literal (Int int) ->
            "(lit " ++ String.fromInt int ++ ")"

        Literal (Float float) ->
            "(lit " ++ String.fromFloat float ++ ")"

        Literal (String string) ->
            "(lit " ++ string ++ ")"
