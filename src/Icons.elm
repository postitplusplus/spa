module Icons exposing (delete, edit)

import Html exposing (Html, img)
import Html.Attributes exposing (class, src)


edit : Html a
edit =
    img
        [ src "/res/edit.svg"
        , class "w-8"
        ]
        []


delete : Html a
delete =
    img
        [ src "/res/delete.svg"
        , class "w-8"
        ]
        []
