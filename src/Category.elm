module Category exposing
    ( Category
    , emptyCategory
    )


type alias Category =
    { name : String
    , toRemove : String
    }


emptyCategory : Category
emptyCategory =
    { name = "New Category"
    , toRemove = ""
    }
