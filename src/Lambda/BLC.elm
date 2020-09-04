module Lambda.BLC exposing (decode, encode)

import Base exposing (..)
import Base.Binary as Binary exposing (Bit(..))
import Lambda.DeBruijn as Term exposing (Term(..))


decode_int : Int -> List Bit -> Try String ( Term, List Bit )
decode_int acc bits =
    case bits of
        [] ->
            Try.fail "unexpected end of input"

        Zero :: rest ->
            Try.succeed ( Var acc, rest )

        One :: rest ->
            decode_int (acc + 1) rest


decode_aux : List Bit -> Try String ( Term, List Bit )
decode_aux bits =
    case bits of
        Zero :: Zero :: rest ->
            decode_aux rest
                |> Try.map (\( body, leftover ) -> ( Abs body, leftover ))

        Zero :: One :: rest ->
            Try.do (decode_aux rest) <| \( abs, leftover ) ->
            Try.do (decode_aux leftover) <| \( arg, leftover2 ) ->
            Try.succeed ( App abs arg, leftover2 )

        One :: rest ->
            decode_int 0 rest

        _ ->
            Try.fail "unexpected input"


decode : List Bit -> Try String Term
decode bits =
    decode_aux bits
        |> Try.map \( term, leftover ) -> term


encode : Term -> List Bit
encode term =
    case term of
        Abs body ->
            Zero :: Zero :: encode body

        App abs arg ->
            Zero :: One :: encode abs ++ encode arg

        Var index ->
            let
                ones =
                    List.repeat (index + 1) One
            in
            ones ++ [ Zero ]
