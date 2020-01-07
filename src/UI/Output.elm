module UI.Output exposing (Args, render)

import Binary
import BLC
import Codec
import DeBruijn
import Dict
import Element exposing (Element)
import Expr


type alias Args =
    { env : Expr.Env
    , error : Maybe String
    }


greeting = """
Hi!

Syntax:
Variable: 1, two, t-h-r-e, "even with spaces"
Abstraction: (lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))
Application: (normalise term)
Extending environment: (define term1 (reduce-once term))

Built-ins:
(bytelist hello) -> builds 8-tuple list
(nat 7) -> builds a Church numeral
(normalise term) -> attempts to reduce a term to it's normal form
(reduce-once term) -> attempts to perform a single beta reduction
term -> expression in the editor
"""


render : Args -> Element msg
render { env, error } =
    let
        content =
            case ( error, Dict.get "acc" env ) of
                ( Just reason, _ ) ->
                    Element.text reason

                ( _, Just expr ) ->
                    let
                        text =
                            expr
                                |> Expr.toLambda
                                |> Maybe.andThen DeBruijn.fromLambda
                                |> Maybe.andThen Codec.decode
                                |> Maybe.map (\decoded -> "Decoded: " ++ Codec.toString decoded)
                                |> Maybe.withDefault (Expr.toString expr)
                    in
                    Element.text text

                _ ->
                    Element.text greeting
    in
    Element.el
        [ Element.height Element.fill
        , Element.width Element.fill
        ]
        content
