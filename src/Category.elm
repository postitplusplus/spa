module Category exposing
    ( Category
    , emptyCategory
    )


type alias Category =
    { id : Int
    , name : String
    }


emptyCategory : Int -> Category
emptyCategory id =
    { id = id
    , name = "New Category"
    }
