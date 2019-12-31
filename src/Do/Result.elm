module Do.Result exposing (do, match)

import Do exposing (..)


do : Result error a -> (a -> Result error b) -> Result error b
do =
    flip Result.andThen


match : (error -> c) -> (a -> c) -> Result error a -> c
match error ok result =
    case result of
        Ok value ->
            ok value

        Err reason ->
            error reason
