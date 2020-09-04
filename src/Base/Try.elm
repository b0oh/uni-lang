module Base.Try exposing (..)

import Base


type alias Try r a=
    Base.Try r a


and_then cont try =
    case try of
        Base.Success value ->
            cont value

        Base.Failure err ->
            Base.Failure err


do =
    Base.flip and_then


fail =
    Base.Failure


map fn try =
    case try of
        Base.Success value ->
            succeed (fn value)

        Base.Failure err ->
            fail err


succeed =
    Base.Success
