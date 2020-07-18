module Base.IO exposing (..)

import Base exposing (..)
import Base.IO.Parser as Parser exposing (Parser)
-- parser vs decoder?


type Action
    = Done
    | Failed String
    | None
    | ReadLine
    | Write String


type State
    = State (Parser ( State, Action ))


type IO a
    = IO ((a -> ( State, Action )) -> ( State, Action ))


do : IO a -> (a -> IO b) -> IO b
do (IO io_cont) cont =
    IO
        (\next ->
            io_cont
                (\value ->
                    let
                        (IO io_cont2) =
                            cont value
                    in
                    io_cont2 next
                )
        )


return : a -> IO a
return a =
    IO
        (\next ->
            ( Parser.succeed a
                |> Parser.map next
                |> State
            , None
            )
        )


read_line : IO String
read_line =
    IO
        (\next ->
            ( Parser.input
                |> Parser.map next
                |> State
            , ReadLine
            )
        )


write : String -> IO ()
write output =
    IO
        (\next ->
            ( Parser.succeed ()
                |> Parser.map next
                |> State
            , Write output
            )
        )


write_line : String -> IO ()
write_line line =
    write (line ++ "\n")


fail : String -> IO ()
fail message =
    IO
        (\_ ->
            ( Parser.fail message
                |> State
            , Failed message
            )
        )


start : IO a -> ( State, Action )
start (IO cont) =
    let
        done _ =
            ( Parser.fail "Done"
                  |> State
            , Done
            )
    in
    cont done


step : Optional String -> State -> ( State, Action )
step input (State parser) =
    case Parser.run input parser of
        Success ( next_state, next_action ) ->
            ( next_state
            , next_action
            )

        Failure reason ->
            let
                qwe =
                    Debug.log "reason" reason
            in
            ( State parser
            , Failed "parsing failed"
            )
