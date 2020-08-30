module Lambda.Parser.Sexp exposing (parse)

import Base exposing (..)
import Base.List as List
import Base.Try as Try
import Sexp exposing (Sexp)
import Lambda.Term as Term exposing (Term)


from_sexp : Sexp -> Try String Term
from_sexp sexp =
    let
        make_abs syms body =
            let
                prepare : List Sexp -> Try String (List String)
                prepare args =
                    case args of
                        [] ->
                            Try.succeed []

                        (Sexp.Symbol str) :: rest ->
                            rest
                                |> prepare
                                |> Try.map (List.cons str)

                        _ ->
                            Try.fail "some of bindings is not a symbol"
            in
            Try.do (prepare syms) <| \bindings ->
            Try.do (from_sexp body) <| \new_body ->
            Try.succeed (Term.make_abs_chain bindings new_body)

        make_app exprs =
            let
                step exprs_ acc =
                    case exprs_ of
                        [] ->
                            Try.succeed acc

                        expr :: rest ->
                            Try.do (from_sexp expr) <| \term ->
                            step rest (Term.App acc term)
            in
            case exprs of
                abs :: arg :: args ->
                    Try.do (from_sexp abs) <| \abs_term ->
                    Try.do (from_sexp arg) <| \arg_term ->
                    step args (Term.App abs_term arg_term)

                _ ->
                    Try.fail "too few terms applied"
    in
    case sexp of
        Sexp.Symbol sym ->
            Try.succeed (Term.Var sym)

        Sexp.List ((abs :: arg :: _) as tokens) ->
            let
                strings =
                    List.map
                        (\token ->
                             case token of
                                 Sexp.Symbol sym ->
                                     sym

                                 _ ->
                                     ""
                        )
                        tokens
            in
            case List.elem_index "->" strings of
                None ->
                    make_app tokens

                Some index ->
                    case List.split_at index tokens of
                        ( args, [ arrow_token, body ] ) ->
                            make_abs args body

                        _ ->
                            Try.fail "syntax error in lambda definition"

        _ ->
            Try.fail "syntax error"


parse : String -> Try String Term
parse input =
    Sexp.fromString input
        |> from_result
        |> Try.and_then from_sexp
