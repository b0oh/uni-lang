module Base exposing (baseEnv)

import DeBruijn
import Codec
import Dict
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


baseEnv =
    Dict.fromList
        [ ( "acc", Expr.Var "nil" )
        , ( "bytelist", Expr.SymbolicMacro bytelistRewriter )
        , ( "eval", Expr.Builtin eval )
        , ( "nat", Expr.SymbolicMacro natRewriter )
        , ( "term", Expr.Var "nil" )
        ]
