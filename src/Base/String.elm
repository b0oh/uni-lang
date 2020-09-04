module Base.String exposing (..)

import Base exposing (..)


from_char =
    String.fromChar


from_int =
    String.fromInt


from_list =
    String.fromList


to_int =
    String.toInt >> from_maybe


to_list =
    String.toList
