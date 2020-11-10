module Icons exposing
    ( add
    , cancel
    , color
    , delete
    , edit
    , expand
    , plus
    , shrink
    , validate
    )

import Html exposing (Html, img, span)
import Html.Attributes exposing (src)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, points, viewBox)


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


add : List (Html.Attribute a) -> Html a
add attr =
    span attr [ img [ src "/res/add.svg" ] [] ]


expand : List (Html.Attribute a) -> Html a
expand attr =
    span
        attr
        [ svg
            [ viewBox "0 0 306 306"
            ]
            [ Svg.g []
                [ Svg.polygon
                    [ points "270.3,58.65 153,175.95 35.7,58.65 0,94.35 153,247.35 306,94.35"
                    , fill "currentColor"
                    ]
                    []
                ]
            ]
        ]


shrink : List (Html.Attribute a) -> Html a
shrink attr =
    span
        attr
        [ svg
            [ viewBox "0 0 306 306"
            ]
            [ Svg.g []
                [ Svg.polygon
                    [ points "153,58.65 0,211.65 35.7,247.35 153,130.05 270.3,247.35 306,211.65"
                    , fill "currentColor"
                    ]
                    []
                ]
            ]
        ]


plus : List (Html.Attribute a) -> Html a
plus attr =
    span
        attr
        [ svg
            [ viewBox "0 0 24 24"
            , Svg.Attributes.class "inline w-8 h-8"
            ]
            [ path [ d "M19,13H13V19H11V13H5V11H11V5H13V11H19V13Z", fill "currentColor" ] []
            ]
        ]
