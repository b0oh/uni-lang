module Lambda.Term exposing (..)

type Term
    = Abs String Term
    | App Term Term
    | Var String


make_abs_chain : List String -> Term -> Term
make_abs_chain bindings body =
    let
        step bindings_ acc =
            case bindings_ of
                [] ->
                    acc

                binding :: rest ->
                    step rest (Abs binding acc)
    in
    step (List.reverse bindings) body


to_string : Term -> String
to_string term =
    case term of
        Abs binding body ->
            String.concat
                [ "("
                , binding
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

        Var sym ->
            sym
