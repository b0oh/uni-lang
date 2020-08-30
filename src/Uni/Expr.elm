module Uni.Expr exposing (..)

import Base exposing (..)
import Base.List as List
import Base.Optional as Optional
import Base.Try as Try
import Dict exposing (Dict)
import Lambda.Term as Term exposing (Term)


type alias Env =
    Dict String Expr


type Expr
    = Abs String Expr
    | App Expr Expr
    | Builtin (Env -> List Expr -> Try String ( Expr, Env ))
    | Closure String Expr Env
    | Var String


empty =
    Dict.empty


define =
    Dict.insert


apply : Env -> Expr -> List Expr -> Try String ( Expr, Env )
apply env abs args =
    let
        step expr args2 =
            case args2 of
                [] ->
                    Try.succeed ( expr, env )

                arg :: rest ->
                    case expr of
                        Closure binding body closured_env ->
                            Try.do (eval env arg) <| \( new_arg, _ ) ->
                            Try.do (eval (define binding new_arg closured_env) body) <| \( new_expr, _ ) ->
                            step new_expr rest

                        _ ->
                            Try.fail "can't apply"
    in
    case abs of
        Builtin builtin ->
            builtin env args

        (Closure _ _ _) as closure ->
            step closure args

        _ ->
            Try.fail "can't apply"


eval : Env -> Expr -> Try String ( Expr, Env )
eval env expr =
    case expr of
        Abs binding body ->
            Try.succeed ( Closure binding body env, env )

        App (Var "quote") value ->
            Try.succeed ( value, env )

        (App _ _) as app ->
            let
                take_args term acc =
                    case term of
                        App abs arg ->
                            take_args abs (List.cons arg acc)

                        _ ->
                            ( term, acc )

                ( abs2, args ) =
                    take_args app []
            in
            Try.do (eval env abs2) <| \( abs3, env2 ) ->
            Try.do (apply env2 abs3 args) <| \( value, env3 ) ->
            Try.succeed ( value, env3 )

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

        Term.Var sym ->
            Var sym


to_term : Expr -> Optional Term
to_term expr =
    case expr of
        Abs binding body ->
            to_term body
                |> Optional.map (Term.Abs binding)

        App abs arg ->
            Optional.do (to_term abs) <| \new_abs ->
            Optional.do (to_term arg) <| \new_arg ->
            Some (Term.App new_abs new_arg)

        Closure binding body env ->
            let
                step bound closured_expr =
                    case closured_expr of
                        Var sym ->
                            if List.member sym bound then
                                Some (Term.Var sym)

                            else
                                Dict.get sym env
                                    |> from_maybe
                                    |> Optional.and_then to_term

                        Abs sym abs_body ->
                            step (sym :: bound) abs_body
                                |> Optional.map (Term.Abs sym)

                        App abs arg ->
                            Optional.do (step bound abs) <| \new_abs ->
                            Optional.do (step bound arg) <| \new_arg ->
                            Some (Term.App new_abs new_arg)

                        _ ->
                            to_term closured_expr
            in
            step [ binding ] body
                |> Optional.map (Term.Abs binding)

        Var sym ->
            Some (Term.Var sym)

        _ ->
            None


to_string expr =
    case to_term expr of
        Some term ->
            Term.to_string term

        _ ->
            "Expr"
