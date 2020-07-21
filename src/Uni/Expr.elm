module Uni.Expr exposing (..)

import Base exposing (..)
import Base.Optional as Opt
import Base.Try as Try
import Dict exposing (Dict)
import Uni.Term as Term exposing (Literal, Term)


type alias Env =
    Dict String Expr


type Expr
    = Abs String Expr
    | App Expr Expr
    | Builtin (Env -> Expr -> Try String Expr)
    | Closure String Expr Env
    | Literal Term.Literal
    | Var String


empty_env =
    Dict.empty


apply : Env -> Expr -> Expr -> Try String Expr
apply env abs arg =
    case abs of
        Builtin builtin ->
            builtin env arg

        Closure binding body closured_env ->
            Try.do (eval env arg) <| \( new_arg, _ ) ->
            Try.do (eval (Dict.insert binding new_arg closured_env) body) <| \( expr, _ ) ->
            Try.succeed expr

        _ ->
            Try.fail "can't apply"


eval : Env -> Expr -> Try String ( Expr, Env )
eval env expr =
    case expr of
        Abs binding body ->
            Try.succeed ( Closure binding body env, env )

        App (Var "quote") value ->
            Try.succeed ( value, env )

        App abs arg ->
            Try.do (eval env abs) <| \( new_abs, _ ) ->
            Try.do (apply env new_abs arg) <| \new_expr ->
            Try.succeed ( new_expr, env )

        Var binding ->
            case Dict.get binding env of
                Just value ->
                    Try.succeed ( value, env )

                _ ->
                    Try.fail (binding ++ " not bound")

        _ ->
            Try.succeed ( expr, env )


from_term : Term -> Expr
from_term term =
    case term of
        Term.Abs binding body ->
            Abs binding (from_term body)

        Term.App abs arg ->
            App (from_term abs) (from_term arg)

        Term.Literal literal ->
            Literal literal

        Term.Symbol sym ->
            Var sym


to_term : Expr -> Optional Term
to_term expr =
    case expr of
        Abs binding body ->
            to_term body
                |> Opt.map (Term.Abs binding)

        App abs arg ->
            Opt.do (to_term abs) <| \new_abs ->
            Opt.do (to_term arg) <| \new_arg ->
            Some (Term.App new_abs new_arg)

        Closure binding body env ->
            let
                step bound closured_expr =
                    case closured_expr of
                        Var sym ->
                            if List.member sym bound then
                                Some (Term.Symbol sym)

                            else
                                Dict.get sym env
                                    |> from_maybe
                                    |> Opt.and_then to_term

                        Abs sym abs_body ->
                            step (sym :: bound) abs_body
                                |> Opt.map (Term.Abs sym)

                        App abs arg ->
                            Opt.do (step bound abs) <| \new_abs ->
                            Opt.do (step bound arg) <| \new_arg ->
                            Some (Term.App new_abs new_arg)

                        _ ->
                            to_term closured_expr
            in
            step [ binding ] body
                |> Opt.map (Term.Abs binding)

        Var sym ->
            Some (Term.Symbol sym)

        _ ->
            None


to_string expr =
    case to_term expr of
        Some term ->
            Term.to_string term

        _ ->
            "Expr"
