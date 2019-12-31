module BLC exposing (decode, encode)

import Binary exposing (Bit(..))
import DeBruijn exposing (Term(..))


decodeInt : Int -> List Bit -> Result String ( Term, List Bit )
decodeInt acc bits =
    case bits of
        [] ->
            Err "unexpected end of input"

        Zero :: rest ->
            Ok ( Var acc, rest )

        One :: rest ->
            decodeInt (acc + 1) rest


decodeAux : List Bit -> Result String ( Term, List Bit )
decodeAux bits =
    case bits of
        Zero :: Zero :: rest ->
            case decodeAux rest of
                Ok ( body, leftover ) ->
                    Ok ( Abs body, leftover )

                error ->
                    error

        Zero :: One :: rest ->
            case decodeAux rest of
                Ok ( abs, absLeftover ) ->
                    case decodeAux absLeftover of
                        Ok ( arg, leftover ) ->
                            Ok ( App abs arg, leftover )

                        error ->
                            error

                error ->
                    error

        One :: rest ->
            decodeInt 0 rest

        _ ->
            Err "unexpected input"


decode : List Bit -> Result String Term
decode bits =
    case decodeAux bits of
        Ok ( term, leftover ) ->
            Ok term

        Err reason ->
            Err reason


encode : Term -> List Bit
encode term =
    case term of
        Var index ->
            let
                ones =
                    List.repeat (index + 1) One
            in
            ones ++ [ Zero ]

        Abs body ->
            Zero :: Zero :: encode body

        App abs arg ->
            Zero :: One :: encode abs ++ encode arg
