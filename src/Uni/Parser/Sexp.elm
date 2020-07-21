module Uni.Parser.Sexp exposing (parse)

import Base exposing (..)
import Base.List as List
import Base.Try as Try
import Sexp exposing (Sexp)
import Uni.Term as Term exposing (Literal, Term)


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

        make_app_acc exprs acc =
            case exprs of
                [] ->
                    Try.succeed acc

                expr :: rest ->
                    Try.do (from_sexp expr) <| \term ->
                    make_app_acc rest (Term.App acc term)

        make_app exprs =
            case exprs of
                abs :: arg :: args ->
                    Try.do (from_sexp abs) <| \abs_term ->
                    Try.do (from_sexp arg) <| \arg_term ->
                    make_app_acc args (Term.App abs_term arg_term)

                _ ->
                    Try.fail "too few terms applied"
    in
    case sexp of
        Sexp.Symbol sym ->
            Try.succeed (Term.Symbol sym)

        Sexp.List ((Sexp.Symbol "lambda") :: rest) ->
            case rest of
                [ Sexp.List args, body ] ->
                    make_abs args body

                _ ->
                    Try.fail "syntax error in lambda definition"

        Sexp.List (abs :: arg :: args) ->
            make_app (abs :: arg :: args)

        _ ->
            Try.fail "syntax error"


parse : String -> Try String Term
parse input =
    Sexp.fromString input
        |> from_result
        |> Try.and_then from_sexp
