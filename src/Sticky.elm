module Sticky exposing
    ( Color(..)
    , SplitSticky
    , Sticky
    , emptySticky
    , getSplitStickies
    , getStickyColor
    )

import Html
import Html.Attributes exposing (class)


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


getStickyColor : Sticky -> Html.Attribute a
getStickyColor sticky =
    case sticky.color of
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
