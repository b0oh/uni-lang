module DeBruijn exposing
    ( Term(..)
    , beta
    , fromLambda
    , isAbs
    , isApp
    , isVar
    , shift
    , reduceOnce
    , normalise
    , toLambda
    , toString
    )

import Dict exposing (Dict)
import Do.Maybe as Maybe
import Lambda


type Term
    = Var Int
    | Abs Term
    | App Term Term


shift : Int -> Term -> Term
shift level =
    let
        shiftAbove cutoff term =
            case term of
                Var index ->
                    if index < cutoff then
                        Var index

                    else
                        Var (index + level)

                Abs body ->
                    Abs (shiftAbove (cutoff + 1) body)

                App abs arg ->
                    App (shiftAbove cutoff abs) (shiftAbove cutoff arg)
    in
    shiftAbove 0


beta : Int -> Term -> Term -> Term
beta level subst term =
    case term of
        Var index ->
            if index == level then
                shift level subst

            else if index < level then
                term

            else
                Var (index - 1)

        Abs body ->
            Abs (beta (level + 1) subst body)

        App abs arg ->
            App (beta level subst abs) (beta level subst arg)


fromLambda : Lambda.Term -> Maybe Term
fromLambda lambdaTerm =
    let
        step { depth, env } term =
            case term of
                Lambda.Var name ->
                    env
                        |> Dict.get name
                        |> Maybe.map (\shift_ -> Var (depth - shift_ - 1))

                Lambda.Abs name body ->
                    let
                        newEnv =
                            Dict.insert name depth env
                    in
                        body
                            |> step { depth = depth + 1, env = newEnv }
                            |> Maybe.map Abs

                Lambda.App abs arg ->
                     Maybe.do (step { depth = depth, env = env } abs) <| \newAbs ->
                     Maybe.do (step { depth = depth, env = env } arg) <| \newArg ->
                     Just (App newAbs newArg)
    in
    step { depth = 0, env = Dict.empty } lambdaTerm


isAbs : Term -> Bool
isAbs term =
    case term of
        Abs _ ->
            True

        _ ->
            False


isApp : Term -> Bool
isApp term =
    case term of
        App _ _ ->
            True

        _ ->
            False


isVar : Term -> Bool
isVar term =
    case term of
        Var _ ->
            True

        _ ->
            False


reduceOnce : Term -> Maybe Term
reduceOnce term =
    case term of
        Var _ ->
            Nothing

        Abs body ->
            body
                |> reduceOnce
                |> Maybe.map Abs

        App (Abs body) arg ->
            Just (beta 0 arg body)

        App abs arg ->
            case reduceOnce abs of
                Just newAbs ->
                    Just (App newAbs arg)

                Nothing ->
                    arg
                        |> reduceOnce
                        |> Maybe.map (App abs)


normalise : Term -> Term
normalise term =
    case reduceOnce term of
        Just newTerm ->
            normalise newTerm

        _ ->
            term


toLambda : Term -> Lambda.Term
toLambda dbTerm =
    let
        step depth term =
            let
                symbol =
                    String.fromInt depth
            in
            case term of
                Var index ->
                    depth - index - 1
                        |> String.fromInt
                        |> Lambda.Var

                Abs body ->
                    Lambda.Abs symbol (step (depth + 1) body)

                App abs arg ->
                    Lambda.App (step depth abs) (step depth arg)
    in
    step 0 dbTerm


toString : Term -> String
toString term =
    case term of
        Var int ->
            String.fromInt <| int

        Abs body ->
            "λ " ++ toString body

        App left right ->
            let
                parensise inner =
                    case inner of
                        Var _ ->
                            toString inner

                        _ ->
                            "(" ++ toString inner ++ ")"
            in
            if isApp right then
                toString left ++ " (" ++ toString right ++ ")"

            else if isApp left then
                toString left ++ " " ++ toString right

            else
                parensise left ++ " " ++ parensise right
