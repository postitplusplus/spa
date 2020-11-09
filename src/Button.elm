module Button exposing (button, deleteButton, undoButton)

import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


button : Bool -> String -> a -> Html a
button active txt onClickEvent =
    Html.button
        [ class "px-6 py-2 rounded-full"
        , class "bg-blue-300"
        , class "font-bold text-white uppercase"
        , class "shadow-md hover:shadow-lg"
        , if active then
            class "cursor-pointer"

          else
            class "cursor-not-allowed"
        , onClick onClickEvent
        ]
        [ text txt ]


deleteButton : Bool -> String -> a -> Html a
deleteButton active txt onClickEvent =
    Html.button
        [ class "mx-1"
        , class "px-6 py-2 rounded-full"
        , class "bg-red-700"
        , class "font-bold text-white uppercase"
        , class "shadow-md hover:shadow-lg hover:underline"
        , if active then
            class "cursor-pointer"

          else
            class "cursor-not-allowed"
        , onClick onClickEvent
        ]
        [ text txt ]


undoButton : Bool -> String -> a -> Html a
undoButton active txt onClickEvent =
    Html.button
        [ class "mx-1"
        , class "px-6 py-2 rounded-full"
        , class "bg-blue-700"
        , class "font-bold text-white uppercase"
        , class "shadow-md hover:shadow-lg hover:underline"
        , if active then
            class "cursor-pointer"

          else
            class "cursor-not-allowed"
        , onClick onClickEvent
        ]
        [ text txt ]
