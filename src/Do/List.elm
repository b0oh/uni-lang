module Do.List exposing (make, match)


make : a -> List a -> List a
make =
    (::)


match : b -> (a -> List a -> b) -> List a -> b
match empty notEmpty list =
    case list of
        [] ->
            empty

        head :: tail ->
            notEmpty head tail
