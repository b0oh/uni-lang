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


symbolElement name =
    Element.el
        [ Font.color Colors.white
        , Element.paddingXY 6 3
        ]
        (Element.text name)


varElement : Var -> Element msg
varElement { symbol } =
    Element.el
        [ Element.width Element.fill
        , Background.color Colors.pink
        ]
        (symbolElement symbol)


absElement : Abs -> Element msg
absElement { symbols, body } =
    let
        symbolsElements =
            List.map symbolElement symbols
    in
    Element.column
        [ Background.color Colors.lightBlue
        , Element.width Element.fill
        ]
        [ Element.row [] symbolsElements
        , Element.el
            [ Element.paddingXY 12 8
            , Element.width Element.fill
            ]
            (astElement body)
        ]


appElement : App -> Element msg
appElement { exprs } =
    case exprs of
        abs :: args ->
            Element.column
                [ Background.color Colors.grey
                , Element.paddingXY 12 8
                , Element.width Element.fill
                ]
                [ astElement abs
                , Element.column
                    [ Element.paddingEach { top = 0, right = 0, bottom = 0, left = 12 }
                    , Element.width Element.fill
                    ]
                    (List.map astElement args)
                ]

        _ ->
            Element.el [] (Element.text "wtf")


bindingElement : String -> Ast -> Element msg
bindingElement name body =
    Element.column
        []
        [ Element.el [] (Element.text (name ++ " = "))
        , Element.el [ Element.paddingEach { top = 0, right = 0, bottom = 0, left = 12 } ] (astElement body)
        ]


letElement : Let -> Element msg
letElement { bindings, body } =
   Element.column
       [ Background.color Colors.orange
       , Element.paddingXY 12 8
       , Element.width Element.fill
       , Element.spacing 3
       , Font.color Colors.white
       ]
       [ Element.text "let"
       , Element.column
           [ Element.paddingEach { top = 0, right = 0, bottom = 0, left = 12 } ]
           (List.map (\(name, innerBody) -> bindingElement name innerBody) bindings)
       , Element.text "in"
       , astElement body
       ]


astElement ast =
    let
        inner =
            case ast of
                MakeVar var ->
                    varElement var

                MakeAbs abs ->
                    absElement abs

                MakeApp app ->
                    appElement app

                MakeLet let_ ->
                    letElement let_
     in
     Element.el
         [ Border.shadow { offset = ( 2, 2 ), size = 0, blur = 10, color = Colors.black }
         , Element.width Element.fill
         ]
         inner


render : Args -> Element msg
render { env } =
    let
        content =
            env
                |> Dict.get "term"
                |> Maybe.andThen Expr.toLambda
                |> Maybe.map (Ast.fromLambda >> Ast.letify >> Ast.compactLets >> astElement)
                |> Maybe.withDefault Element.none
    in
    Element.el
        [ Element.padding 8 ]
        content
