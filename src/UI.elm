module UI exposing (render)

import Element
import Element.Font as Font
import Html exposing (Html)
import UI.Editor as Editor
import UI.Output as Output
import UI.Shell as Shell
import UI.Style as Style


render :
    { displayShell : Bool
    , editor : Editor.Args
    , output : Output.Args
    , shell : Shell.Args msg
    }
    -> Html msg
render { displayShell, editor, output, shell } =
    let
        editorUi =
            Editor.render editor

        shellUi =
            if displayShell then
                Shell.render shell

            else
                Element.none

        leftPane =
            Element.column
                [ Element.height Element.fill
                , Element.width Element.fill
                ]
                [ editorUi
                , shellUi
                ]

        outputUi =
            Output.render output

        layout =
            Element.row
                [ Element.height Element.fill
                , Element.width Element.fill
                ]
                [ leftPane
                , outputUi
                ]

        styles =
            Style.default ++ [ Element.padding 6 ]

    in
    Element.layout styles layout
