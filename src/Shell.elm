module Shell exposing (run)

import Base exposing (..)
import Base.IO as IO exposing (IO)
import Base.Optional as Optional
import Base.String as String
import Base.Try as Try
import Lambda.Codec as Codec
import Lambda.DeBruijn as DeBruijn
import Lambda.Parser.Sexp as Parser
import Lambda.Term as Lambda
import Uni.Expr as Expr exposing (Expr)


define env exprs =
    case exprs of
        [ Expr.Var binding, value ] ->
            case Expr.eval env value of
                Success ( value2, env2 ) ->
                    Try.succeed ( value2, Expr.define binding value2 env2 )

                Failure reason ->
                    Try.fail reason

        _ ->
            Try.fail "syntax error in define"


require env exprs =
    Try.fail "require: implement me one day"


defaults =
    Expr.empty
        |> Expr.define "define" (Expr.Builtin define)
        |> Expr.define "require" (Expr.Builtin require)


shell : Expr.Env -> IO Unit
shell env =
    IO.do (IO.write "> ") <| \_ ->
    IO.do IO.read_line <| \input ->
    let
        try =
            Parser.parse input
                |> Try.map Expr.from_term
                |> Try.and_then (Expr.eval env)

        ( output, new_env ) =
            case try of
                Success ( expr, new_env_ ) ->
                    let
                        decoded =
                            expr
                                |> Expr.to_term
                                |> Optional.and_then DeBruijn.from_lambda
                                |> Optional.and_then Codec.decode
                                |> Optional.map
                                    (\decoded2 ->
                                         "\nDecoded: " ++ Codec.to_string decoded2
                                    )
                                |> Optional.with_default ""
                    in
                    ( Expr.to_string expr ++ decoded, new_env_ )

                Failure reason ->
                    ( reason, env )
    in
    IO.do (IO.write_line output) <| \_ ->
    shell new_env


run : IO Unit
run =
    IO.do (IO.write_line "Hi! I am Uni in the shell.") <| \_ ->
    shell defaults
