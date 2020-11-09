module Sticky exposing
    ( Color(..)
    , SplitSticky
    , Sticky
    , colorChip
    , emptySticky
    , getColorAttribute
    , getSplitStickies
    )

import Html exposing (Html, span)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type Color
    = Yellow
    | Peach
    | Blue
    | Green
    | Teal


type alias Sticky =
    { id : Int
    , content : String
    , color : Color
    }


emptySticky : Int -> Sticky
emptySticky id =
    Sticky id "Please edit the sticky's content" Yellow


getColorAttribute : Color -> Html.Attribute a
getColorAttribute color =
    case color of
        Yellow ->
            class "bg-yellow-200"

        Peach ->
            class "bg-orange-300"

        Blue ->
            class "bg-blue-200"

        Green ->
            class "bg-green-200"

        Teal ->
            class "bg-teal-200"


type alias SplitSticky =
    { before : List Sticky
    , current : Sticky
    , after : List Sticky
    }


getSplitStickies : List Sticky -> Int -> SplitSticky
getSplitStickies stickies id =
    let
        mCurrent =
            List.head <|
                List.filter (\s -> s.id == id) stickies

        current =
            case mCurrent of
                Nothing ->
                    emptySticky 0

                Just c ->
                    c

        before =
            List.filter (\s -> s.id < id) stickies

        after =
            List.filter (\s -> s.id > id) stickies
    in
    SplitSticky before current after


colorChip : Color -> a -> Html a
colorChip color action =
    span
        [ getColorAttribute color
        , class "w-4 h-4 rounded-full"
        , class "mx-1"
        , class "cursor-pointer"
        , class "border border-gray-300"
        , class "hover:shadow-outline"
        , onClick action
        ]
        []
