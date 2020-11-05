module Button exposing (button)

import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


button : String -> a -> Html a
button txt onClickEvent =
    Html.button
        [ class "px-6 py-2 rounded-full"
        , class "bg-blue-300"
        , class "font-bold text-white uppercase"
        , class "shadow-md hover:shadow-lg"
        , onClick onClickEvent
        ]
        [ text txt ]
