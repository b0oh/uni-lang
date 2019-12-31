module UI.Style exposing (bg, default, fg)

import Element
import Element.Background as Background
import Element.Font as Font


bg =
    Element.rgb255 36 39 40


fg =
    Element.rgb 1 1 1


default =
    [ Background.color bg
    , Font.color fg
    , Font.size 14
    , Font.family [ Font.monospace ]
    ]
