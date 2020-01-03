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
        [ ( "acc", Expr.Var "nothing" )
        , ( "bytelist", Expr.SymbolicMacro bytelistRewriter )
        , ( "eval", Expr.Builtin eval )
        , ( "nat", Expr.SymbolicMacro natRewriter )
        , ( "term", example )
        ]
