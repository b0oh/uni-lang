module Output exposing (render)

import Element


render =
    Element.text "output"
        |> Element.el
           [ Element.height Element.fill
           , Element.width Element.fill
           ]
