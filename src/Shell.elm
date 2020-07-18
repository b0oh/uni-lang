module Shell exposing (run)

import Base exposing (..)
import Base.IO as IO exposing (IO)
import Base.String as String
import Uni.Parser as Parser
import Uni.Term as Term


shell : IO Unit
shell =
    IO.do (IO.write "> ") <| \_ ->
    IO.do IO.read_line <| \input ->
    let
        output =
            case Parser.parse input of
                Success [term] ->
                    Term.to_string term

                _ ->
                    "error"
    in
    IO.do (IO.write_line output) <| \_ ->
    shell


run : IO Unit
run =
    IO.do (IO.write_line "Hi! I am Uni in the shell.") <| \_ ->
    shell
