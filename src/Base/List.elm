module Base.List exposing (..)

import Base exposing (..)


cons : a -> List a -> List a
cons =
    (::)


find_index : (a -> Bool) -> List a -> Optional Int
find_index =
    let
        step : Int -> (a -> Bool) -> List a -> Optional Int
        step index predicate list =
            case list of
                [] ->
                    None

                x :: xs ->
                    if predicate x then
                        Some index

                    else
                        step (index + 1) predicate xs
    in
    step 0


elem_index : a -> List a -> Optional Int
elem_index x =
    find_index ((==) x)


split_at : Int -> List a -> ( List a, List a )
split_at n xs =
    ( List.take n xs, List.drop n xs )
