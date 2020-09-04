module Lambda.Codec exposing
    ( Decoded(..)
    , decode
    , encode_bitlist
    , encode_byte
    , encode_bytelist
    , encode_charlist
    , encode_const
    , encode_list
    , encode_nat
    , encode_nil
    , encode_pair
    , to_string
    )

import Base exposing (..)
import Base.Binary as Binary
import Base.Char as Char
import Base.List as List
import Base.Optional as Optional
import Base.String as String
import Lambda.DeBruijn as DeBruijn exposing (Term(..))


type Decoded
    = Byte Binary.Byte
    | Charlist String
    | Const
    | List (List Decoded)
    | Nat Int
    | Nil
    | Pair Decoded Decoded
    | Term Term


decode_nil : Term -> Optional Decoded
decode_nil term =
    case term of
        Abs (Abs (Var 0)) ->
            Optional.some Nil

        _ ->
            Optional.none


decode_const : Term -> Optional Decoded
decode_const term =
    case term of
        Abs (Abs (Var 1)) ->
            Optional.some Const

        _ ->
            Optional.none


decode_list_aux term =
    case term of
        Abs (App (App (Var 0) head) tail) ->
            Optional.do (decode head) <| \decoded_head ->
            Optional.do (decode_list_aux tail) <| \decoded_tail ->
            Optional.some (decoded_head :: decoded_tail)

        Abs (Abs (Var 0)) ->
            Optional.some []

        _ ->
            Optional.none


decode_lists : Term -> Optional Decoded
decode_lists term =
    let
        is_printable code =
            code >= 9 && code < 127

        get_code element =
            case element of
                Byte byte ->
                    Binary.byte_to_int byte

                Nat code ->
                    code

                _ ->
                    0
    in
    Optional.do (decode_list_aux term) <| \list ->
    if List.all (get_code >> is_printable) list then
        list
            |> List.map (get_code >> Char.from_code)
            |> String.from_list
            |> Charlist
            |> Optional.some

    else
        Optional.some (List list)


decode_pair : Term -> Optional Decoded
decode_pair term =
    case term of
        Abs (App (App (Var 0) first) second) ->
            let
                decoded_first =
                    first
                        |> decode
                        |> Optional.with_default (Term first)

                decoded_second =
                    second
                        |> decode
                        |> Optional.with_default (Term second)
            in
            Optional.some (Pair decoded_first decoded_second)

        _ ->
            Optional.none


decode_bit : Term -> Optional Binary.Bit
decode_bit term =
    let
        decode_bit_aux bit decoder =
            Optional.map (constant bit) (decoder term)
    in
    decode_bit_aux Binary.Zero decode_nil
        |> Optional.or_then (decode_bit_aux Binary.One decode_const)


decode_byte : Term -> Optional Decoded
decode_byte term =
    case term of
        Abs (App (App (App (App (App (App (App (App (Var 0) first) second) third) fourth) fifth) sixth) seventh) eighth) ->
            Optional.do (decode_bit first) <| \first_ ->
            Optional.do (decode_bit second) <| \second_ ->
            Optional.do (decode_bit third) <| \third_ ->
            Optional.do (decode_bit fourth) <| \fourth_ ->
            Optional.do (decode_bit fifth) <| \fifth_ ->
            Optional.do (decode_bit sixth) <| \sixth_ ->
            Optional.do (decode_bit seventh) <| \seventh_ ->
            Optional.do (decode_bit eighth) <| \eighth_ ->
                { first = first_
                , second = second_
                , third = third_
                , fourth = fourth_
                , fifth = fifth_
                , sixth = sixth_
                , seventh = seventh_
                , eighth = eighth_
                }
                |> Byte
                |> Optional.some


        _ ->
            Optional.none


decode_church_nat : Term -> Optional Decoded
decode_church_nat term =
    case term of
        Abs (Abs (Var 0)) ->
            Optional.some (Nat 0)

        Abs (Abs (App (Var 1) body)) ->
            Optional.do (decode_church_nat (Abs (Abs body))) <| \decoded ->
            case decoded of
                Nat nat ->
                    Optional.some (Nat (nat + 1))

                _ ->
                    Optional.none

        _ ->
            Optional.none


decode : Term -> Optional Decoded
decode term =
    decode_nil term
        |> Optional.or_then (decode_const term)
        |> Optional.or_then (decode_byte term)
        |> Optional.or_then (decode_lists term)
        |> Optional.or_then (decode_pair term)
        |> Optional.or_then (decode_church_nat term)


encode_nat int =
    case int of
        0 ->
            Abs (Abs (Var 0))

        _ ->
            case encode_nat (int - 1) of
                Abs (Abs body) ->
                    Abs (Abs (App (Var 1) body))

                _ ->
                    Var 666


encode_list list =
    case list of
        [] ->
            encode_nil

        head :: tail ->
            encode_pair head (encode_list tail)


encode_bit bit =
    case bit of
        Binary.Zero ->
            encode_nil

        Binary.One ->
            encode_const


encode_bits int =
    let
        bits =
            Binary.from_int int

        length =
            List.length bits

        padded =
            bits ++ (List.repeat (8 - length) Binary.Zero)
    in
    List.map encode_bit padded


encode_byte int =
    let
        bits =
            Binary.from_int int

        length =
            List.length bits

        padded =
            bits ++ (List.repeat (8 - length) Binary.Zero)

        fold_bit bit acc =
            App acc bit
    in
    padded
        |> List.map encode_bit
        |> List.fold_left fold_bit (Var 0)
        |> Abs


encode_vec8_1 int =
    Abs (App (App (App (App (App (App (App (App (Var 0) encode_const) encode_nil) encode_const) encode_nil) encode_const) encode_nil) encode_const) encode_nil)


encode_vec8_2 int =
    encode_pair
        encode_const
        (encode_pair
             encode_nil
             (encode_pair
                  encode_const
                  (encode_pair
                       encode_nil
                       (encode_pair
                            encode_const
                            (encode_pair
                                 encode_nil
                                 (encode_pair
                                      encode_const
                                      (encode_pair
                                           encode_const
                                           encode_nil
                                      )
                                 )
                            )
                       )
                  )
             )
        )


encode_vec8_3 int =
    encode_pair
        (encode_pair
             (encode_pair encode_const encode_nil)
             (encode_pair encode_const encode_nil)
        )
        (encode_pair
             (encode_pair encode_const encode_nil)
             (encode_pair encode_const encode_nil)
        )


encode_bitlist string =
    string
        |> String.to_list
        |> List.concat_map (Char.to_code >> encode_bits)
        |> encode_list


encode_bytelist string =
    string
        |> String.to_list
        |> List.map (Char.to_code >> encode_byte)
        |> encode_list


encode_charlist string =
    string
        |> String.to_list
        |> List.map (Char.to_code >> encode_nat)
        |> encode_list


encode_const =
    Abs (Abs (Var 1))


encode_nil =
    Abs (Abs (Var 0))


encode_pair first second =
    Abs (App (App (Var 0) first) second)


to_string : Decoded -> String
to_string decoded =
    let
        to_bit decoded_ =
            case decoded_ of
                Const ->
                    "1"

                Nil ->
                    "0"

                _ ->
                    ""
    in
    case decoded of
        Byte byte ->
            String.from_int (Binary.byte_to_int byte)

        Charlist string ->
            "\"" ++ string ++ "\""

        Const ->
            "const"

        List list ->
            let
                inner =
                    list
                        |> List.map to_string
                        |> String.join ", "

            in
            "[ " ++ inner ++ " ]"

        Nat num ->
            String.from_int num

        Nil ->
            "nothing"

        Pair first second ->
            "( " ++ to_string first ++ ", " ++ to_string second ++ " )"

        Term term ->
            DeBruijn.to_string term
