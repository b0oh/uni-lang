module Base.String exposing (..)

import Base exposing (..)


to_int =
    String.toInt >> from_maybe
