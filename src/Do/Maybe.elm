module Do.Maybe exposing (do, match, orThen)

import Do exposing (..)


do : Maybe a -> (a -> Maybe b) -> Maybe b
do =
    flip Maybe.andThen


match : b -> (a -> b) -> Maybe a -> b
match nothing just maybe =
    case maybe of
        Just value ->
            just value

        Nothing ->
            nothing


orThen : Maybe a -> Maybe a -> Maybe a
orThen thenCase maybe =
    match thenCase (always maybe) maybe
