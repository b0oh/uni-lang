module Base.Optional exposing (..)

import Base


type alias Optional a =
    Base.Optional a


map : (a -> b) -> Optional a -> Optional b
map fn optional =
    case optional of
        Base.Some value ->
            Base.Some (fn value)

        Base.None ->
            Base.None


with_default : a -> Optional a -> a
with_default default optional =
    case optional of
        Base.Some value ->
            value

        Base.None ->
            default
