module Base exposing (baseEnv)

import Codec
import DeBruijn
import Dict
import Example
import Expr
import Lambda


bytelistRewriter : String -> Result String Lambda.Term
bytelistRewriter string =
    string
        |> Codec.encodeBytelist
        |> DeBruijn.toLambda
        |> Ok


eval : Expr.Env -> Expr.Expr -> Result String Expr.Expr
eval env expr =
    Expr.eval env expr
        |> Result.map Tuple.first


natRewriter : String -> Result String Lambda.Term
natRewriter string =
    let
        maybeNat =
            string
                |> String.toInt
                |> Maybe.map (Codec.encodeNat >> DeBruijn.toLambda)
    in
    case maybeNat of
        Just nat ->
            Ok nat

        Nothing ->
            Err "can't rewrite to nat"


normalise : Expr.Env -> Expr.Expr -> Result String Expr.Expr
normalise env expr =
    let
        wrapUnwrap =
            Tuple.first
                >> Expr.toLambda
                >> Maybe.map (Lambda.normalise >> Expr.fromLambda)
                >> Result.fromMaybe "can't normalise term"
    in
    Expr.eval env expr
        |> Result.andThen wrapUnwrap


reduceOnce : Expr.Env -> Expr.Expr -> Result String Expr.Expr
reduceOnce env expr =
    let
        wrapUnwrap =
            Tuple.first
                >> Expr.toLambda
                >> Maybe.andThen Lambda.reduceOnce
                >> Maybe.map Expr.fromLambda
                >> Result.fromMaybe "can't reduce term"
    in
    Expr.eval env expr
        |> Result.andThen wrapUnwrap


example =
    let
        zero =
            Expr.fromLambda Example.zero

        inc =
            Expr.fromLambda Example.inc
    in
    Expr.App
        (Expr.Abs "zero"
             (Expr.App
                  (Expr.Abs "inc"
                       (Expr.App (Expr.Var "inc") (Expr.Var "zero"))
                  )
                  inc
             )
        )
        zero


baseEnv =
    Dict.fromList
        [ ( "bytelist", Expr.SymbolicMacro bytelistRewriter )
        , ( "eval", Expr.Builtin eval )
        , ( "nat", Expr.SymbolicMacro natRewriter )
        , ( "normalise", Expr.Builtin normalise )
        , ( "reduce-once", Expr.Builtin reduceOnce )
        , ( "term", example )
        ]
