module Base.Optional exposing (..)

import Base


type alias Optional a =
    Base.Optional a


and_then cont optional =
    case optional of
        Base.Some value ->
            cont value

        Base.None ->
            Base.None


do =
    Base.flip and_then


map : (a -> b) -> Optional a -> Optional b
map fn optional =
    case optional of
        Base.Some value ->
            Base.Some (fn value)

        Base.None ->
            Base.None


none =
    Base.None


or_then : Optional a -> Optional a -> Optional a
or_then optional then_ =
    case optional of
        Base.None ->
            then_

        _ ->
            optional


some =
    Base.Some


with_default : a -> Optional a -> a
with_default default optional =
    case optional of
        Base.Some value ->
            value

        Base.None ->
            default
