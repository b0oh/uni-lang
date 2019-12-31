module Ast exposing (..)

import Lambda exposing (Term)


type alias Var =
    { symbol : String
    , highlighted : Bool
    }


type alias Abs =
    { symbols : List String
    , body : Ast
    , highlighted : Bool
    }


type alias App =
    { exprs : List Ast
    , isRedex : Bool
    , highlighted : Bool
    }


type alias Let =
    { bindings : List ( String, Ast )
    , body : Ast
    , highlighted : Bool
    }


type Ast
    = MakeVar Var
    | MakeAbs Abs
    | MakeApp App
    | MakeLet Let


fromLambda : Term -> Ast
fromLambda term =
    case term of
        Lambda.Var sym ->
            MakeVar
                { symbol = sym
                , highlighted = False
                }

        Lambda.Abs sym body ->
            MakeAbs
                { symbols = [ sym ]
                , body = fromLambda body
                , highlighted = False
                }

        Lambda.App abs arg ->
            let
                isRedex =
                    case abs of
                        Lambda.Abs _ _ ->
                            True

                        _ ->
                            False
            in
            MakeApp
                { exprs = [ fromLambda abs, fromLambda arg ]
                , isRedex = isRedex
                , highlighted = False
                }


letify : Ast -> Ast
letify ast =
    case ast of
        MakeApp app ->
            if app.isRedex then
                case app.exprs of
                    (MakeAbs { symbols, body }) :: arg :: [] ->
                        let
                            symbol =
                                symbols
                                    |> List.head
                                    |> Maybe.withDefault "wtf1"
                        in
                        MakeLet
                            { bindings = [ ( symbol, arg )]
                            , body = letify body
                            , highlighted = False
                            }

                    _ ->
                        MakeVar { symbol = "wtf2", highlighted = False }

            else
                MakeApp { app | exprs = List.map letify app.exprs }

        MakeAbs abs ->
            MakeAbs { abs | body = letify abs.body }

        _ ->
            ast


compactLets : Ast -> Ast
compactLets ast =
    case ast of
        MakeVar _ ->
            ast

        MakeAbs abs ->
            case compactLets abs.body of
                MakeAbs { symbols, body, highlighted } ->
                    MakeAbs
                        { symbols = abs.symbols ++ symbols
                        , body = body
                        , highlighted = abs.highlighted || highlighted
                        }

                newBody ->
                    MakeAbs { abs | body = newBody }

        MakeApp app ->
            let
                abs =
                    app.exprs
                        |> List.head
                        |> Maybe.map compactLets

                args =
                    app.exprs
                        |> List.tail
                        |> Maybe.withDefault []
            in
            case abs of
                Just (MakeApp innerApp) ->
                    MakeApp { app | exprs = innerApp.exprs ++ args }

                _ ->
                    MakeApp { app | exprs = List.map compactLets app.exprs }

        MakeLet let_ ->
            let
                compactBindings =
                    List.map (\( name, body ) -> ( name, compactLets body ))
            in
            case compactLets let_.body of
                MakeLet { bindings, body } ->
                    MakeLet
                        { let_
                            | bindings = compactBindings (let_.bindings ++ bindings)
                            , body = body
                        }

                body ->
                    MakeLet
                        { let_
                            | bindings = compactBindings let_.bindings
                            , body = body
                        }
