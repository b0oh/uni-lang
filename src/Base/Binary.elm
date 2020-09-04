module Base.Binary exposing
    ( Bit(..)
    , Byte
    , byte_to_int
    , from_int
    , from_string
    , to_string
    )

import Base.Int as Int
import Base.List as List
import Base.String as String


type Bit
    = Zero
    | One


type alias Byte =
    { first : Bit
    , second : Bit
    , third : Bit
    , fourth : Bit
    , fifth : Bit
    , sixth : Bit
    , seventh : Bit
    , eighth : Bit
    }


bit_to_int : Bit -> Int
bit_to_int bit =
    case bit of
        Zero ->
            0

        One ->
            1


byte_to_int : Byte -> Int
byte_to_int { first, second, third, fourth, fifth, sixth, seventh, eighth } =
    (bit_to_int first)
        + (bit_to_int second) * 2
        + (bit_to_int third) * 4
        + (bit_to_int fourth) * 8
        + (bit_to_int fifth) * 16
        + (bit_to_int sixth) * 32
        + (bit_to_int seventh) * 64
        + (bit_to_int eighth) * 128


from_int : Int -> List Bit
from_int int =
    let
        step acc int2 =
            let
                bit =
                    if Int.remainder_by 2 int2 == 0 then
                        Zero

                    else
                        One

                acc2 =
                    bit :: acc

                next =
                    int2 // 2
            in
            if next > 0 then
                step acc2 next

            else
                List.reverse acc2
    in
    step [] int


from_string : String -> List Bit
from_string string =
    let
        map_char char =
            case char of
                '0' ->
                    [ Zero ]

                '1' ->
                    [ One ]

                _ ->
                    []
    in
    string
        |> String.to_list
        |> List.concat_map map_char


to_string : List Bit -> String
to_string bits =
    let
        map_bit bit =
            case bit of
                Zero ->
                    '0'

                One ->
                    '1'
    in
    bits
        |> List.map map_bit
        |> String.from_list
