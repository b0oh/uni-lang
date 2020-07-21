module Base.List exposing (..)

cons : a -> List a -> List a
cons =
    (::)
