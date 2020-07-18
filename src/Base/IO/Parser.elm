module Base.IO.Parser exposing (..)

import Base exposing (..)
import Base.Optional as Optional
import Json.Decode as Decode
import Json.Encode as Encode


type alias Error =
    Decode.Error


type alias Parser a =
    Decode.Decoder a


fail =
    Decode.fail


input =
    Decode.string


map =
    Decode.map


run : (Optional String) -> Parser a -> Try Error a
run optional_input parser =
    let
        value =
            optional_input
                |> Optional.map Encode.string
                |> Optional.with_default Encode.null
    in
    case Decode.decodeValue parser value of
        Ok result ->
            Success result

        Err reason ->
            Failure reason


succeed =
    Decode.succeed
