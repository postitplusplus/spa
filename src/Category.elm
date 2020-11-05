module Category exposing
    ( Category
    , SplitCategories
    , emptyCategory
    , getSpliCategories
    )


type alias Category =
    { id : Int
    , name : String
    }


emptyCategory : Int -> Category
emptyCategory id =
    { id = id
    , name = String.fromInt id ++ " - new category"
    }


type alias SplitCategories =
    { before : List Category
    , current : Category
    , after : List Category
    }


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
