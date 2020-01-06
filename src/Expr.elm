module Expr exposing (Env, Expr(..), apply, eval, fromLambda, toLambda, toString)

import DeBruijn
import Dict exposing (Dict)
import Do.Maybe as Maybe
import Do.Result as Result
import Lambda


type alias Env =
    Dict String Expr


type Expr
    = Var String
    | Abs String Expr
    | App Expr Expr
    | Closure String Expr Env
    | Builtin (Env -> Expr -> Result String Expr)
    | SymbolicMacro (String -> Result String Lambda.Term)


apply : Env -> Expr -> Expr -> Result String Expr
apply env abs arg =
    case abs of
        Builtin builtin ->
            builtin env arg

        Closure binding body closuredEnv ->
            Result.do (eval env arg) <| \( newArg, _ ) ->
            Result.do (eval (Dict.insert binding newArg closuredEnv) body) <| \( expr, _ ) ->
            Ok expr

        SymbolicMacro rewriter ->
            case arg of
                Var symbol ->
                    Result.do (rewriter symbol) <| \term ->
                    Ok (fromLambda term)

                _ ->
                    Err "symbolic macro application requires a symbol to be applied"


        _ ->
            Ok (App abs arg)


eval : Env -> Expr -> Result String ( Expr, Env )
eval env expr =
    case expr of
        Var sym ->
            case Dict.get sym env of
                Just value ->
                    Ok ( value, env )

                _ ->
                    Err (sym ++ " not bound")

        Abs sym body ->
            Ok ( Closure sym body env, env )

        App (Var "quote") value ->
            Ok ( value, env )

        App (App (Var "define") (Var name)) value ->
            Result.do (eval env value) <| \( newValue, _ ) ->
            Ok ( newValue, Dict.insert name newValue env )

        App abs arg ->
            Result.do (eval env abs) <| \( newAbs, _ ) ->
            Result.do (apply env newAbs arg) <| \newExpr ->
            Ok ( newExpr, env )

        _ ->
            Ok ( expr, env )


fromLambda : Lambda.Term -> Expr
fromLambda term =
    case term of
        Lambda.Var sym ->
            Var sym

        Lambda.Abs sym body ->
            Abs sym (fromLambda body)

        Lambda.App abs arg ->
            App (fromLambda abs) (fromLambda arg)


toLambda : Expr -> Maybe Lambda.Term
toLambda expr =
    case expr of
        Var sym ->
            Just (Lambda.Var sym)

        Abs sym body ->
            toLambda body
                |> Maybe.map (Lambda.Abs sym)

        App abs arg ->
            Maybe.do (toLambda abs) <| \newAbs ->
            Maybe.do (toLambda arg) <| \newArg ->
            Just (Lambda.App newAbs newArg)

        Closure binding body env ->
            let
                step bound closuredExpr =
                    case closuredExpr of
                        Var sym ->
                            if List.member sym bound then
                                Just (Lambda.Var sym)

                            else
                                Dict.get sym env
                                    |> Maybe.andThen toLambda

                        Abs sym absBody ->
                            step (sym :: bound) absBody
                                |> Maybe.map (Lambda.Abs sym)

                        App abs arg ->
                            Maybe.do (step bound abs) <| \newAbs ->
                            Maybe.do (step bound arg) <| \newArg ->
                            Just (Lambda.App newAbs newArg)

                        _ ->
                            toLambda closuredExpr
            in
            step [ binding ] body
                |> Maybe.map (Lambda.Abs binding)

        _ ->
            Nothing


toString expr =
    case toLambda expr of
        Just term ->
            Lambda.toString term

        _ ->
            "Expr"
