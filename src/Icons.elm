module Icons exposing (cancel, delete, edit, validate)

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


validate : Html a
validate =
    img
        [ src "/res/validate.svg"
        , class "w-8"
        ]
        []


cancel : Html a
cancel =
    img
        [ src "/res/cancel.svg"
        , class "w-8"
        ]
        []
