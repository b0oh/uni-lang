module Binary exposing
    ( Bit(..)
    , Byte
    , byteToInt
    , fromInt
    , fromString
    , toString
    )

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


bitToInt : Bit -> Int
bitToInt bit =
    case bit of
        Zero ->
            0

        One ->
            1


byteToInt : Byte -> Int
byteToInt { first, second, third, fourth, fifth, sixth, seventh, eighth } =
    (bitToInt first)
        + (bitToInt second) * 2
        + (bitToInt third) * 4
        + (bitToInt fourth) * 8
        + (bitToInt fifth) * 16
        + (bitToInt sixth) * 32
        + (bitToInt seventh) * 64
        + (bitToInt eighth) * 128


fromInt : Int -> List Bit
fromInt int =
    let
        step acc int_ =
            let
                bit =
                    if remainderBy 2 int_ == 0 then
                        Zero

                    else
                        One

                newAcc =
                    bit :: acc

                next =
                    int_ // 2
            in
            if next > 0 then
                step newAcc next

            else
                List.reverse newAcc
    in
    step [] int


fromString : String -> List Bit
fromString string =
    let
        mapChar char =
            case char of
                '0' ->
                    [ Zero ]

                '1' ->
                    [ One ]

                _ ->
                    []
    in
    string
        |> String.toList
        |> List.concatMap mapChar


toString : List Bit -> String
toString bits =
    let
        mapBit bit =
            case bit of
                Zero ->
                    '0'

                One ->
                    '1'
    in
    bits
        |> List.map mapBit
        |> String.fromList
