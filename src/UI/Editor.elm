module UI.Editor exposing (Args, render)

import Ast exposing (Ast(..), Var, Abs, App, Let)
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Expr
import UI.Style.Colors as Colors

type alias Args =
    { env : Expr.Env
    }


padLeft =
    Element.paddingEach
        { top = 0
        , right = 0
        , bottom = 0
        , left = 12
        }


shadow =
    Border.shadow
        { offset = ( 2, 2 )
        , size = 0
        , blur = 4
        , color = Colors.black
        }


renderSymbol name =
    Element.el
        [ Font.color Colors.white
        , Element.paddingXY 6 3
        ]
        (Element.text name)


renderVar : Var -> Element msg
renderVar { symbol } =
    Element.el
        [ Background.color Colors.pink
        , Element.width Element.fill
        , shadow
        ]
        (renderSymbol symbol)


renderAbs : Abs -> Element msg
renderAbs { symbols, body } =
    let
        renderedSymbols =
            List.map renderSymbol symbols
    in
    Element.column
        [ Background.color Colors.lightBlue
        , Element.width Element.fill
        , shadow
        ]
        [ Element.row [] renderedSymbols
        , Element.el
            [ Element.paddingXY 12 8
            , Element.width Element.fill
            ]
            (renderAst body)
        ]


renderApp : App -> Element msg
renderApp { exprs } =
    case exprs of
        abs :: args ->
            Element.column
                [ Element.width Element.fill ]
                [ renderAst abs
                , Element.column
                    [ padLeft
                    , Element.width Element.fill
                    ]
                    (List.map renderAst args)
                ]

        _ ->
            Element.el [] (Element.text "wtf")


renderBinding : String -> Ast -> Element msg
renderBinding name body =
    let
        ( bindingName, bindingBody ) =
            case body of
                MakeAbs abs ->
                    ( name ++ " " ++ String.join " " abs.symbols ++ " = "
                    , abs.body
                    )

                _ ->
                    ( name ++ " = "
                    , body
                    )
    in
    Element.column
        [ Element.spacing 6
        , Element.paddingXY 0 3
        ]
        [ Element.el [] (Element.text bindingName)
        , Element.el [ padLeft ] (renderAst bindingBody)
        ]


renderLet : Let -> Element msg
renderLet { bindings, body } =
    let
        renderedBindings =
            bindings
                |> List.map (\(name, innerBody) -> renderBinding name innerBody)
                |> Element.column [ padLeft ]
    in
    Element.column
        [ Element.paddingXY 12 8
        , Element.spacing 3
        , Element.width Element.fill
        , Font.color Colors.white
        ]
        [ Element.text "let"
        , renderedBindings
        , Element.text "in"
        , renderAst body
        ]


renderAst ast =
    let
        inner =
            case ast of
                MakeVar var ->
                    renderVar var

                MakeAbs abs ->
                    renderAbs abs

                MakeApp app ->
                    renderApp app

                MakeLet let_ ->
                    renderLet let_
     in
     Element.el
         [ Element.width Element.fill ]
         inner


render : Args -> Element msg
render { env } =
    let
        mapTerm =
            Ast.fromLambda >> Ast.letify >> Ast.compactLets >> renderAst

        content =
            env
                |> Dict.get "term"
                |> Maybe.andThen Expr.toLambda
                |> Maybe.map mapTerm
                |> Maybe.withDefault Element.none
    in
    Element.el
        [ Element.padding 8 ]
        content
