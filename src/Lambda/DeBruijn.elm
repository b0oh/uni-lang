module Lambda.DeBruijn exposing
    ( Term(..)
    , beta
    , from_lambda
    , is_abs
    , is_app
    , is_var
    , shift
    , reduce_once
    , normalise
    , to_lambda
    , to_string
    )

import Base exposing (..)
import Base.Optional as Optional
import Base.String as String
import Base.Try as Try
import Dict exposing (Dict)
import Lambda.Term as Lambda


type Term
    = Var Int
    | Abs Term
    | App Term Term


shift : Int -> Term -> Term
shift level =
    let
        shift_above cutoff term =
            case term of
                Abs body ->
                    Abs (shift_above (cutoff + 1) body)

                App abs arg ->
                    App (shift_above cutoff abs) (shift_above cutoff arg)

                Var index ->
                    if index < cutoff then
                        Var index

                    else
                        Var (index + level)

    in
    shift_above 0


beta : Int -> Term -> Term -> Term
beta level subst term =
    case term of
        Abs body ->
            Abs (beta (level + 1) subst body)

        App abs arg ->
            App (beta level subst abs) (beta level subst arg)

        Var index ->
            if index == level then
                shift level subst

            else if index < level then
                term

            else
                Var (index - 1)


from_lambda : Lambda.Term -> Optional Term
from_lambda lambda_term =
    let
        step { depth, env } term =
            case term of
                Lambda.Abs name body ->
                    let
                        env2 =
                            Dict.insert name depth env
                    in
                        body
                            |> step { depth = depth + 1, env = env2 }
                            |> Optional.map Abs

                Lambda.App abs arg ->
                     Optional.do (step { depth = depth, env = env } abs) <| \abs2 ->
                     Optional.do (step { depth = depth, env = env } arg) <| \arg2 ->
                     Optional.some (App abs2 arg2)

                Lambda.Var name ->
                    env
                        |> Dict.get name
                        |> from_maybe
                        |> Optional.map (\shift2 -> Var (depth - shift2 - 1))
    in
    step { depth = 0, env = Dict.empty } lambda_term


is_abs : Term -> Bool
is_abs term =
    case term of
        Abs _ ->
            True

        _ ->
            False


is_app : Term -> Bool
is_app term =
    case term of
        App _ _ ->
            True

        _ ->
            False


is_var : Term -> Bool
is_var term =
    case term of
        Var _ ->
            True

        _ ->
            False


reduce_once : Term -> Optional Term
reduce_once term =
    case term of
        Abs body ->
            body
                |> reduce_once
                |> Optional.map Abs

        App (Abs body) arg ->
            Optional.some (beta 0 arg body)

        App abs arg ->
            case reduce_once abs of
                Some abs2 ->
                    Optional.some (App abs2 arg)

                None ->
                    arg
                        |> reduce_once
                        |> Optional.map (App abs)

        Var _ ->
            None


normalise : Term -> Term
normalise term =
    case reduce_once term of
        Some term2 ->
            normalise term2

        _ ->
            term


to_lambda : Term -> Lambda.Term
to_lambda db_term =
    let
        step depth term =
            let
                symbol =
                    String.from_int depth
            in
            case term of
                Abs body ->
                    Lambda.Abs symbol (step (depth + 1) body)

                App abs arg ->
                    Lambda.App (step depth abs) (step depth arg)

                Var index ->
                    depth - index - 1
                        |> String.from_int
                        |> Lambda.Var
    in
    step 0 db_term


to_string : Term -> String
to_string term =
    case term of
        Var int ->
            String.from_int int

        Abs body ->
            "λ " ++ to_string body

        App left right ->
            let
                parensise inner =
                    case inner of
                        Var _ ->
                            to_string inner

                        _ ->
                            "(" ++ to_string inner ++ ")"
            in
            if is_app right then
                to_string left ++ " (" ++ to_string right ++ ")"

            else if is_app left then
                to_string left ++ " " ++ to_string right

            else
                parensise left ++ " " ++ parensise right
