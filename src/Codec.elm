module Codec exposing
    ( Decoded(..)
    , decode
    , encodeBitlist
    , encodeByte
    , encodeBytelist
    , encodeCharlist
    , encodeConst
    , encodeList
    , encodeNat
    , encodeNil
    , encodePair
    , toString
    )

import Binary
import DeBruijn exposing (Term(..))
import Do.Maybe as Maybe


type Decoded
    = Byte Binary.Byte
    | Charlist String
    | Const
    | List (List Decoded)
    | Nat Int
    | Nil
    | Pair Decoded Decoded
    | Term Term


decodeNil : Term -> Maybe Decoded
decodeNil term =
    case term of
        Abs (Abs (Var 0)) ->
            Just Nil

        _ ->
            Nothing


decodeConst : Term -> Maybe Decoded
decodeConst term =
    case term of
        Abs (Abs (Var 1)) ->
            Just Const

        _ ->
            Nothing


decodeListAux term =
    case term of
        Abs (App (App (Var 0) head) tail) ->
            Maybe.do (decode head) <| \decodedHead ->
            Maybe.do (decodeListAux tail) <| \decodedTail ->
            Just (decodedHead :: decodedTail)

        Abs (Abs (Var 0)) ->
            Just []

        _ ->
            Nothing


decodeLists : Term -> Maybe Decoded
decodeLists term =
    let
        isPrintable code =
            code >= 9 && code < 127

        getCode element =
            case element of
                Byte byte ->
                    Binary.byteToInt byte

                Nat code ->
                    code

                _ ->
                    0
    in
    Maybe.do (decodeListAux term) <| \list ->
    if List.all (getCode >> isPrintable) list then
        list
            |> List.map (getCode >> Char.fromCode)
            |> String.fromList
            |> Charlist
            |> Just

    else
        Just (List list)


decodePair : Term -> Maybe Decoded
decodePair term =
    case term of
        Abs (App (App (Var 0) first) second) ->
            let
                decodedFirst =
                    first
                        |> decode
                        |> Maybe.withDefault (Term first)

                decodedSecond =
                    second
                        |> decode
                        |> Maybe.withDefault (Term second)
            in
            Just (Pair decodedFirst decodedSecond)

        _ ->
            Nothing


decodeBit : Term -> Maybe Binary.Bit
decodeBit term =
    let
        decodeBitAux bit decoder =
            Maybe.map (always bit) (decoder term)
    in
    decodeBitAux Binary.Zero decodeNil
        |> Maybe.orThen (decodeBitAux Binary.One decodeConst)


decodeByte : Term -> Maybe Decoded
decodeByte term =
    case term of
        Abs (App (App (App (App (App (App (App (App (Var 0) first) second) third) fourth) fifth) sixth) seventh) eighth) ->
            Maybe.do (decodeBit first) <| \first_ ->
            Maybe.do (decodeBit second) <| \second_ ->
            Maybe.do (decodeBit third) <| \third_ ->
            Maybe.do (decodeBit fourth) <| \fourth_ ->
            Maybe.do (decodeBit fifth) <| \fifth_ ->
            Maybe.do (decodeBit sixth) <| \sixth_ ->
            Maybe.do (decodeBit seventh) <| \seventh_ ->
            Maybe.do (decodeBit eighth) <| \eighth_ ->
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
                |> Just


        _ ->
            Nothing


decodeChurchNat : Term -> Maybe Decoded
decodeChurchNat term =
    case term of
        Abs (Abs (Var 0)) ->
            Just (Nat 0)

        Abs (Abs (App (Var 1) body)) ->
            case decodeChurchNat (Abs (Abs body)) of
                Just (Nat nat) ->
                    Just (Nat (nat + 1))

                _ ->
                    Nothing

        _ ->
            Nothing


decode : Term -> Maybe Decoded
decode term =
    decodeNil term
        |> Maybe.orThen (decodeConst term)
        |> Maybe.orThen (decodeByte term)
        |> Maybe.orThen (decodeLists term)
        |> Maybe.orThen (decodePair term)
        |> Maybe.orThen (decodeChurchNat term)


encodeNat int =
    case int of
        0 ->
            Abs (Abs (Var 0))

        _ ->
            case encodeNat (int - 1) of
                Abs (Abs body) ->
                    Abs (Abs (App (Var 1) body))

                _ ->
                    Var 666


encodeList list =
    case list of
        [] ->
            encodeNil

        head :: tail ->
            encodePair head (encodeList tail)


encodeBit bit =
    case bit of
        Binary.Zero ->
            encodeNil

        Binary.One ->
            encodeConst


encodeBits int =
    let
        bits =
            Binary.fromInt int

        length =
            List.length bits

        padded =
            bits ++ (List.repeat (8 - length) Binary.Zero)
    in
    List.map encodeBit padded


encodeByte int =
    let
        bits =
            Binary.fromInt int

        length =
            List.length bits

        padded =
            bits ++ (List.repeat (8 - length) Binary.Zero)

        foldBit bit acc =
            App acc bit
    in
    padded
        |> List.map encodeBit
        |> List.foldl foldBit (Var 0)
        |> Abs


encodeVec1 int =
    Abs (App (App (App (App (App (App (App (App (Var 0) encodeConst) encodeNil) encodeConst) encodeNil) encodeConst) encodeNil) encodeConst) encodeNil)


encodeVec2 int =
    encodePair
        encodeConst
        (encodePair
             encodeNil
             (encodePair
                  encodeConst
                  (encodePair
                       encodeNil
                       (encodePair
                            encodeConst
                            (encodePair
                                 encodeNil
                                 (encodePair
                                      encodeConst
                                      (encodePair
                                           encodeConst
                                           encodeNil
                                      )
                                 )
                            )
                       )
                  )
             )
        )


encodeVec3 int =
    encodePair
        (encodePair
             (encodePair encodeConst encodeNil)
             (encodePair encodeConst encodeNil)
        )
        (encodePair
             (encodePair encodeConst encodeNil)
             (encodePair encodeConst encodeNil)
        )


encodeBitlist string =
    string
        |> String.toList
        |> List.concatMap (Char.toCode >> encodeBits)
        |> encodeList


encodeBytelist string =
    string
        |> String.toList
        |> List.map (Char.toCode >> encodeByte)
        |> encodeList


encodeCharlist string =
    string
        |> String.toList
        |> List.map (Char.toCode >> encodeNat)
        |> encodeList


encodeConst =
    Abs (Abs (Var 1))


encodeNil =
    Abs (Abs (Var 0))


encodePair first second =
    Abs (App (App (Var 0) first) second)


toString : Decoded -> String
toString decoded =
    let
        toBit decoded_ =
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
            String.fromInt (Binary.byteToInt byte)

        Charlist string ->
            "\"" ++ string ++ "\""

        Const ->
            "const"

        List list ->
            let
                inner =
                    list
                        |> List.map toString
                        |> String.join ", "

            in
            "[ " ++ inner ++ " ]"

        Nat num ->
            String.fromInt num

        Nil ->
            "nothing"

        Pair first second ->
            "( " ++ toString first ++ ", " ++ toString second ++ " )"

        Term term ->
            DeBruijn.toString term
