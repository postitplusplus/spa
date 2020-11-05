module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Button exposing (button, deleteButton)
import Category
    exposing
        ( Category
        , emptyCategory
        , getSpliCategories
        )
import Html exposing (Html, div, header, input, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Icons
import Json.Decode as D
import Sticky exposing (Sticky, getSplitStickies)
import Url exposing (Url)



--- MAIN ---


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }



--- MODEL ---


type alias EditingCategoryData =
    { before : List Category
    , after : List Category
    , current : Category
    , initial : Category
    }


type alias DeleteCategoryData =
    { before : List Category
    , after : List Category
    , current : Category
    }


type alias EditingStickyColorData =
    { before : List Sticky
    , after : List Sticky
    , current : Sticky
    }


type Model
    = Viewing (List Category)
    | EditingCategory EditingCategoryData
    | DeletingCategory DeleteCategoryData
    | EditingStickyColor EditingCategoryData EditingStickyColorData


init : D.Value -> Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( Viewing [], Cmd.none )


onUrlRequest : UrlRequest -> Msg
onUrlRequest urlRequest =
    ClickedLink urlRequest


onUrlChange : Url -> Msg
onUrlChange url =
    ChangedUrl url



--- UPDATE ---


type Msg
    = ClickedLink UrlRequest
    | ChangedUrl Url
      -- Create Category
    | CreateCategory
      -- Delete Category
    | SetDeleteCategoryMode Int
    | ConfirmCategoryDeletion
      -- Edit Category name
    | SetEditMode Int
    | EditCategoryName String
    | SaveCategoryName
    | CancelCategoryName
      -- Add note
    | AddNote Category
      -- Change Color
    | SetChangeColorMode Category Sticky


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            performUrlRequest urlRequest model

        ( ChangedUrl _, _ ) ->
            ( model, Cmd.none )

        -- Create Category
        ( CreateCategory, Viewing categories ) ->
            ( Viewing (categories ++ [ emptyCategory (List.length categories) ]), Cmd.none )

        ( CreateCategory, _ ) ->
            ( model, Cmd.none )

        -- Add Note
        ( AddNote category, Viewing categories ) ->
            let
                updatedCategory =
                    Category.addNewSticky category

                split =
                    Category.getSpliCategories categories category.id
            in
            ( Viewing (split.before ++ updatedCategory :: split.after), Cmd.none )

        ( AddNote _, _ ) ->
            ( model, Cmd.none )

        -- Category Edition
        ( SetEditMode id, Viewing categories ) ->
            let
                split =
                    Category.getSpliCategories categories id

                data =
                    EditingCategoryData split.before split.after split.current split.current
            in
            ( EditingCategory data, Cmd.none )

        ( SetEditMode _, _ ) ->
            ( model, Cmd.none )

        ( EditCategoryName name, EditingCategory data ) ->
            let
                cur =
                    data.current

                newCurrent =
                    { cur | name = name }
            in
            ( EditingCategory { data | current = newCurrent }, Cmd.none )

        ( EditCategoryName _, _ ) ->
            ( model, Cmd.none )

        ( SaveCategoryName, EditingCategory data ) ->
            ( Viewing (data.before ++ data.current :: data.after), Cmd.none )

        ( SaveCategoryName, _ ) ->
            ( model, Cmd.none )

        ( CancelCategoryName, EditingCategory data ) ->
            ( Viewing (data.before ++ data.initial :: data.after), Cmd.none )

        ( CancelCategoryName, _ ) ->
            ( model, Cmd.none )

        -- Delete Category
        ( SetDeleteCategoryMode id, Viewing categories ) ->
            let
                split =
                    Category.getSpliCategories categories id

                data =
                    DeleteCategoryData split.before split.after split.current
            in
            ( DeletingCategory data, Cmd.none )

        ( SetDeleteCategoryMode _, _ ) ->
            ( model, Cmd.none )

        ( ConfirmCategoryDeletion, DeletingCategory data ) ->
            let
                after =
                    List.map (\c -> { c | id = c.id - 1 }) data.after
            in
            ( Viewing (data.before ++ after), Cmd.none )

        ( ConfirmCategoryDeletion, _ ) ->
            ( model, Cmd.none )

        -- Change Sticky color
        ( SetChangeColorMode category sticky, Viewing categories ) ->
            let
                cs =
                    getSpliCategories categories category.id

                categoryData =
                    EditingCategoryData cs.before cs.after cs.current cs.current

                ss =
                    getSplitStickies category.stickies sticky.id

                stickyData =
                    EditingStickyColorData ss.before ss.after ss.current
            in
            ( EditingStickyColor categoryData stickyData, Cmd.none )

        ( SetChangeColorMode _ _, _ ) ->
            ( model, Cmd.none )


performUrlRequest : UrlRequest -> Model -> ( Model, Cmd msg )
performUrlRequest request model =
    case request of
        Internal _ ->
            ( model, Cmd.none )

        External url ->
            ( model, Nav.load url )



--- VIEW ---


view : Model -> Document Msg
view model =
    let
        title =
            "Post it, plus plus"

        body =
            case model of
                Viewing categories ->
                    mainView <| viewNormal categories

                EditingCategory data ->
                    mainView <| viewEditingCategory data.before data.current data.after

                DeletingCategory data ->
                    mainView <| viewDeletingCategory data.before data.current data.after

                EditingStickyColor categoryData stickyData ->
                    mainView <| viewChangeStickyColor categoryData stickyData
    in
    { title = title, body = [ body ] }


mainView : Html Msg -> Html Msg
mainView page =
    div
        [ class "w-full"
        , class "flex flex-col"
        , class "items-stretch"
        ]
        [ noteHeader
        , createCategory
        , page
        ]


viewNormal : List Category -> Html Msg
viewNormal categories =
    div [] (List.map viewCategory categories)


viewEditingCategory : List Category -> Category -> List Category -> Html Msg
viewEditingCategory before current after =
    let
        b =
            List.map viewCategory before

        a =
            List.map viewCategory after

        c =
            viewEditCategory current
    in
    div
        []
        (b ++ c :: a)


viewDeletingCategory : List Category -> Category -> List Category -> Html Msg
viewDeletingCategory before current after =
    let
        b =
            List.map viewCategory before

        a =
            List.map viewCategory after

        c =
            viewDeleteCategory current
    in
    div
        []
        (b ++ c :: a)


noteHeader : Html Msg
noteHeader =
    header
        [ class "h-24"
        , class "bg-blue-300"
        , class "text-6xl font-bold text-center text-gray-100 uppercase"
        ]
        [ text "My Notes" ]


createCategory : Html Msg
createCategory =
    div
        [ class "p-8" ]
        [ button "Create Category" CreateCategory
        ]



--- View category


viewCategory : Category -> Html Msg
viewCategory category =
    div
        [ class "w-full"
        , class "my-4"
        , class "bg-blue-100"
        , class "flex flex-col"
        ]
        [ viewCategoryHeader category
        , viewStickies category category.stickies
        ]


viewCategoryHeader : Category -> Html Msg
viewCategoryHeader category =
    div
        [ class "flex flex-row"
        , class "items-center"
        , class "px-4 py-2"
        ]
        [ div
            [ class "uppercase h-100"
            , class "text-3xl font-bold text-gray-500"
            ]
            [ text category.name ]
        , div [ class "flex-grow" ] []
        , div
            [ class "flex flex-row"
            , class "items-center"
            ]
            [ span [ class "m-2 mr-6" ] [ button "Add note" (AddNote category) ]
            , Icons.edit
                [ class "w-8 m-2 cursor-pointer"
                , onClick <| SetEditMode category.id
                ]
            , Icons.delete
                [ class "w-8 m-2 cursor-pointer"
                , onClick <| SetDeleteCategoryMode category.id
                ]
            ]
        ]


viewStickies : Category -> List Sticky -> Html Msg
viewStickies category stickies =
    div
        [ class "flex flow-row"
        , class "overflow-x-auto"
        ]
        (List.map (viewSticky category) stickies)


viewSticky : Category -> Sticky -> Html Msg
viewSticky category sticky =
    div
        [ class "w-64 h-64 p-4 m-4 min-w-64 min-h-64"
        , class "flex flex-col"
        , class "shadow"
        , Sticky.getStickyColor sticky
        ]
        [ div
            [ class "flex flex-row pb-2" ]
            [ span [ class "flex-grow" ] []
            , Icons.color
                [ class "w-6 mx-1 cursor-pointer"
                , onClick (SetChangeColorMode category sticky)
                ]
            , Icons.edit
                [ class "w-6 mx-1 cursor-pointer"
                ]
            , Icons.delete
                [ class "w-6 cursor-pointer"
                ]
            ]
        , div
            [ class "break-all"
            , class "overflow-y-auto"
            ]
            [ text sticky.content ]
        ]



--- Edit category


viewEditCategory : Category -> Html Msg
viewEditCategory category =
    div
        [ class "w-full"
        , class "my-4"
        , class "bg-blue-300"
        , class "flex flex-col"
        ]
        [ viewEditCategoryHeader category
        , viewStickies category category.stickies
        ]


viewEditCategoryHeader : Category -> Html Msg
viewEditCategoryHeader category =
    div
        [ class "flex flex-row"
        , class "items-center"
        , class "px-4 py-2"
        ]
        [ input
            [ class "uppercase h-100"
            , class "px-4"
            , class "text-3xl font-bold text-gray-500"
            , class "flex-grow"
            , class "rounded-full shadow-inner"
            , Html.Attributes.value category.name
            , Html.Events.onInput EditCategoryName
            ]
            []
        , div
            [ class "flex flex-row"
            , class "items-center"
            ]
            [ Icons.validate
                [ class "w-8 m-2 cursor-pointer"
                , onClick SaveCategoryName
                ]
            , Icons.cancel
                [ class "w-8 m-2 cursor-pointer"
                , onClick CancelCategoryName
                ]
            ]
        ]



--- Deleting Category


viewDeleteCategory : Category -> Html Msg
viewDeleteCategory category =
    let
        warning =
            div
                [ class "w-2/3 m-auto my-4"
                , class "p-4"
                , class "bg-red-500"
                , class "rounded-md"
                , class "text-xl font-bold text-white"
                , class "text-center"
                ]
                [ text "You are about to delete a category."
                , Html.br [] []
                , text "Associated notes will also be deleted."
                , Html.br [] []
                , Html.br [] []
                , deleteButton "Confirm deletion" ConfirmCategoryDeletion
                ]
    in
    div
        [ class "w-full"
        , class "my-4"
        , class "bg-red-300"
        , class "flex flex-col"
        ]
        [ viewDeleteCategoryHeader category
        , warning
        , viewStickies category category.stickies
        ]


viewDeleteCategoryHeader : Category -> Html Msg
viewDeleteCategoryHeader category =
    div
        [ class "flex flex-row"
        , class "items-center"
        , class "px-4 py-2"
        ]
        [ div
            [ class "uppercase h-100"
            , class "text-3xl font-bold text-gray-100"
            ]
            [ text category.name ]
        , div [ class "flex-grow" ] []
        ]


viewChangeStickyColor : EditingCategoryData -> EditingStickyColorData -> Html Msg
viewChangeStickyColor catData stickyData =
    let
        before =
            List.map viewCategory catData.before

        current =
            viewChangeStickyColorCategory catData.current stickyData

        after =
            List.map viewCategory catData.after
    in
    div [] (before ++ current :: after)


viewChangeStickyColorCategory : Category -> EditingStickyColorData -> Html Msg
viewChangeStickyColorCategory category stickyData =
    let
        before =
            List.map (viewSticky category) stickyData.before

        after =
            List.map (viewSticky category) stickyData.after

        current =
            viewChangeStickyColorSticky stickyData.current

        stickyList =
            div
                [ class "flex flow-row"
                , class "overflow-x-auto"
                ]
                (before ++ current :: after)
    in
    div
        [ class "w-full"
        , class "my-4"
        , class "bg-blue-100"
        , class "flex flex-col"
        ]
        [ viewCategoryHeader category
        , stickyList
        ]


viewChangeStickyColorSticky : Sticky -> Html Msg
viewChangeStickyColorSticky sticky =
    div
        [ class "w-64 h-64 p-4 m-4 min-w-64 min-h-64"
        , class "flex flex-col"
        , class "shadow"
        , Sticky.getStickyColor sticky
        ]
        [ div
            [ class "flex flex-row pb-2" ]
            [ span [ class "flex-grow" ] []
            ]
        , div
            [ class "break-all"
            , class "overflow-y-auto"
            ]
            [ text sticky.content ]
        ]



-- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
