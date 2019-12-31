module Events exposing (onEnter)

import Element
import Html.Events
import Json.Decode as Decode


onEnter : msg -> Element.Attribute msg
onEnter message =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed message

            else
                Decode.fail ""
    in
    Decode.andThen isEnter Html.Events.keyCode
        |> Html.Events.on "keydown"
        |> Element.htmlAttribute
