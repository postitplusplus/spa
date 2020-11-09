module Icons exposing (cancel, color, delete, edit, validate)

import Html exposing (Html, img, span)
import Html.Attributes exposing (src)


edit : List (Html.Attribute a) -> Html a
edit attr =
    span attr [ img [ src "/res/edit.svg" ] [] ]


delete : List (Html.Attribute a) -> Html a
delete attr =
    span attr [ img [ src "/res/delete.svg" ] [] ]


validate : List (Html.Attribute a) -> Html a
validate attr =
    span attr [ img [ src "/res/validate.svg" ] [] ]


cancel : List (Html.Attribute a) -> Html a
cancel attr =
    span attr [ img [ src "/res/cancel.svg" ] [] ]


color : List (Html.Attribute a) -> Html a
color attr =
    span attr [ img [ src "/res/color.svg" ] [] ]
