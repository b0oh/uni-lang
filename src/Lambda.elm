module Lambda exposing
    ( Term(..)
    , beta
    , isAbs
    , isApp
    , isRedex
    , isVar
    , makeAbsChain
    , normalise
    , reduceOnce
    , toString
    )

type Term
    = Var String
    | Abs String Term
    | App Term Term


beta : String -> Term -> Term -> Term
beta name subst term =
    case term of
        Var sym ->
            if sym == name then
                subst

            else
                term

        Abs sym body ->
            if sym == name then
                term

            else
                Abs sym (beta name subst body)

        App abs arg ->
            App (beta name subst abs) (beta name subst arg)


isAbs : Term -> Bool
isAbs term =
    case term of
        Abs _ _ ->
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


isRedex : Term -> Bool
isRedex term =
    case term of
        App (Abs _ _) _ ->
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



makeAbsChain : List String -> Term -> Term
makeAbsChain args body =
    let
        step subArgs acc =
            case subArgs of
                [] ->
                    acc

                id :: rest ->
                    step rest (Abs id acc)
    in
    step (List.reverse args) body



reduceOnce : Term -> Maybe Term
reduceOnce term =
    case term of
        Var _ ->
            Nothing

        Abs sym body ->
            body
                |> reduceOnce
                |> Maybe.map (Abs sym)

        App (Abs sym body) arg ->
            Just (beta sym arg body)

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



toString : Term -> String
toString term =
    let
        printAbs inner =
            case inner of
                Abs sym body ->
                    sym ++ " " ++ printAbs body

                _ ->
                    "→ " ++ toString inner
    in
    case term of
        Var sym ->
            sym

        (Abs _ _) as abs ->
            printAbs abs

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
