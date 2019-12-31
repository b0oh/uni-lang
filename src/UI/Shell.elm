module UI.Shell exposing (Args, render)

import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Events
import Json.Decode as Decode
import UI.Style as Style

type alias Args msg =
    { onChange : String -> msg
    , onEnter : msg
    , text : String
    }


render : Args msg  -> Element msg
render { onChange, onEnter, text } =
    Input.text
        [ Background.color Style.bg
        , Font.color Style.fg
        , Events.onEnter onEnter
        ]
        { label = Input.labelHidden "shell"
        , onChange = onChange
        , placeholder = Nothing
        , text = text
        }
