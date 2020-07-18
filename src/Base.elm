module Base
    exposing
    ( Optional(..)
    , Try(..)
    , Unit
    , constant
    , flip
    , from_maybe
    )

type Optional x
    = Some x
    | None -- I would rather use nothing but it conflicts with elm stdlib


type Try failure success
    = Failure failure
    | Success success


type alias Unit =
    ()


constant always skip =
    always


flip fun a b =
    fun b a


from_maybe : Maybe a -> Optional a
from_maybe maybe =
    case maybe of
        Just value ->
            Some value

        Nothing ->
            None


identity x =
    x
