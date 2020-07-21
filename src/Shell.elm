module Shell exposing (run)

import Base exposing (..)
import Base.IO as IO exposing (IO)
import Base.String as String
import Base.Try as Try
import Uni.Expr as Expr exposing (Expr)
import Uni.Parser.Sexp as Parser
import Uni.Term as Term


eval : Expr.Env -> Expr -> Try String ( Expr, Expr.Env )
eval env expr =
    Expr.eval env expr


shell : Expr.Env -> IO Unit
shell env =
    IO.do (IO.write "> ") <| \_ ->
    IO.do IO.read_line <| \input ->
    let
        try =
            Parser.parse input
                |> Try.map Expr.from_term
                |> Try.and_then (eval env)

        ( output, new_env ) =
            case try of
                Success ( expr, new_env_ ) ->
                    ( Expr.to_string expr, new_env_ )

                Failure reason ->
                    ( reason, env )
    in
    IO.do (IO.write_line output) <| \_ ->
    shell new_env


run : IO Unit
run =
    IO.do (IO.write_line "Hi! I am Uni in the shell.") <| \_ ->
    shell Expr.empty_env
