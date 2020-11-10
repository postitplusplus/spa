module Category exposing
    ( Category
    , SplitCategories
    , addNewSticky
    , emptyCategory
    , getSpliCategories
    , placeholder
    , rmSticky
    )

import Sticky exposing (Sticky)


type alias Category =
    { id : Int
    , name : String
    , stickies : List Sticky
    , open : Bool
    }


emptyCategory : Int -> Category
emptyCategory id =
    { id = id
    , name = ""
    , stickies = []
    , open = True
    }


type alias SplitCategories =
    { before : List Category
    , current : Category
    , after : List Category
    }


placeholder : String
placeholder =
    "Edit the category"


getSpliCategories : List Category -> Int -> SplitCategories
getSpliCategories categories id =
    let
        mCurrent =
            List.head <|
                List.drop id categories

        current =
            case mCurrent of
                Nothing ->
                    emptyCategory 0

                Just c ->
                    c

        before =
            List.take id categories

        after =
            List.drop (id + 1) categories
    in
    SplitCategories before current after


newSticky : Category -> Sticky
newSticky category =
    let
        maxId =
            List.foldr (\s max -> Basics.max s.id max) 0 category.stickies
    in
    Sticky.emptySticky (maxId + 1)


addNewSticky : Category -> Category
addNewSticky category =
    let
        sticky =
            newSticky category
    in
    { category | stickies = category.stickies ++ [ sticky ] }


rmSticky : Category -> Int -> Category
rmSticky category id =
    let
        newStickies =
            List.filter (\s -> s.id == id) category.stickies
    in
    { category | stickies = newStickies }
