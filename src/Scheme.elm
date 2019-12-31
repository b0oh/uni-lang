module Scheme exposing (fromSexp)

import Do.List as List
import Do.Result as Result
import Lambda
import Sexp exposing (Sexp)


fromSexp : Sexp -> Result String Lambda.Term
fromSexp sexp =
    let
        prepareArgs : List Sexp -> Result String (List String)
        prepareArgs subArgs =
            case subArgs of
                [] ->
                    Ok []

                (Sexp.Symbol str) :: rest ->
                    rest
                        |> prepareArgs
                        |> Result.map (List.make str)

                _ ->
                    Err "some of args are not symbol"

        makeAbs args body =
            Result.do (prepareArgs args) <| \syms ->
            Result.do (fromSexp body) <| \subBody ->
            Ok (Lambda.makeAbsChain syms subBody)

        makeAppAcc exprs acc =
            case exprs of
                [] ->
                    Ok acc

                expr :: rest ->
                    Result.do (fromSexp expr) <| \term ->
                    makeAppAcc rest (Lambda.App acc term)

        makeApp exprs =
            case exprs of
                abs :: arg :: args ->
                    Result.do (fromSexp abs) <| \absTerm ->
                    Result.do (fromSexp arg) <| \argTerm ->
                    makeAppAcc args (Lambda.App absTerm argTerm)

                _ ->
                    Err "too few terms applied"
    in
    case sexp of
        Sexp.Symbol sym ->
            Ok (Lambda.Var sym)

        Sexp.List ((Sexp.Symbol "lambda") :: rest) ->
            case rest of
                [ Sexp.List args, body ] ->
                    makeAbs args body

                _ ->
                    Err "syntax error in lambda definition"

        Sexp.List (abs :: arg :: args) ->
            makeApp (abs :: arg :: args)

        _ ->
            Err "syntax error"
