module Helpers exposing
    ( EditingCategoryData
    , EditingStickyData
    , updateCurrentCategory
    )

import Category exposing (Category)
import Sticky exposing (Sticky)


type alias EditingCategoryData =
    { before : List Category
    , after : List Category
    , current : Category
    , initial : Category
    }


type alias EditingStickyData =
    { before : List Sticky
    , after : List Sticky
    , current : Sticky
    , initial : Sticky
    }


updateCurrentCategory : EditingCategoryData -> Sticky -> EditingCategoryData
updateCurrentCategory data sticky =
    let
        current =
            data.current

        splitStickies =
            Sticky.getSplitStickies current.stickies sticky.id

        newCurrent =
            { current | stickies = splitStickies.before ++ (sticky :: splitStickies.after) }
    in
    { data | current = newCurrent }
