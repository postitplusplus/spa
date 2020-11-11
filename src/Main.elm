module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom
import Browser.Navigation as Nav exposing (Key)
import Button exposing (button, deleteButton, undoButton)
import Category
    exposing
        ( Category
        , emptyCategory
        , getSpliCategories
        )
import Helpers
    exposing
        ( EditingCategoryData
        , EditingStickyData
        )
import Html exposing (Html, div, header, input, span, text, textarea)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Icons
import Json.Decode as D
import Sticky exposing (Sticky, getSplitStickies)
import Task
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


type Model
    = Viewing (List Category)
    | EditingCategory EditingCategoryData
    | DeletingCategory EditingCategoryData
    | EditingStickyColor EditingCategoryData EditingStickyData
    | EditingStickyContent EditingCategoryData EditingStickyData
    | DeletingSticky EditingCategoryData EditingStickyData


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
    | ToggleCategory Int
      -- Delete Category
    | SetDeleteCategoryMode Int
    | ConfirmCategoryDeletion
    | UndoCategoryDeletion
      -- Edit Category name
    | SetEditMode Int
    | EditCategoryName String
    | SaveCategoryName
    | CancelCategoryName
      -- Add note
    | AddNote Category
      -- Change Color
    | SetChangeColorMode Category Sticky
    | SelectStickyColor Sticky.Color
      -- Edit Sticky content
    | SetEditStickyMode Category Sticky
    | EditStickyContent String
    | SaveStickyContent
    | CancelStickyContent
      -- Delete Sticky
    | SetDeleteStickyMode Category Sticky
    | ValidateStickyDeletion
    | CancelStickyDeletion
      -- NoOp
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            performUrlRequest urlRequest model

        ( ChangedUrl _, _ ) ->
            ( model, Cmd.none )

        ( NoOp, _ ) ->
            ( model, Cmd.none )

        -- Create Category
        ( CreateCategory, Viewing categories ) ->
            ( Viewing (categories ++ [ emptyCategory (List.length categories) ]), Cmd.none )

        ( CreateCategory, _ ) ->
            ( model, Cmd.none )

        ( ToggleCategory id, Viewing categories ) ->
            let
                currentCategories =
                    Category.getSpliCategories categories id

                current =
                    currentCategories.current

                newCurrent =
                    { current | open = not current.open }

                newCategories =
                    currentCategories.before ++ newCurrent :: currentCategories.after
            in
            ( Viewing newCategories, Cmd.none )

        ( ToggleCategory _, _ ) ->
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
            ( EditingCategory data, Task.attempt (\_ -> NoOp) (Browser.Dom.focus "currentCategory") )

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
                    EditingCategoryData split.before split.after split.current split.current
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

        ( UndoCategoryDeletion, DeletingCategory data ) ->
            ( Viewing (data.before ++ data.current :: data.after), Cmd.none )

        ( UndoCategoryDeletion, _ ) ->
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
                    EditingStickyData ss.before ss.after ss.current ss.current
            in
            ( EditingStickyColor categoryData stickyData, Cmd.none )

        ( SetChangeColorMode _ _, _ ) ->
            ( model, Cmd.none )

        ( SelectStickyColor color, EditingStickyColor categoryData stickyData ) ->
            let
                sticky =
                    stickyData.current

                newSticky =
                    { sticky | color = color }

                current =
                    categoryData.current

                newCurrent =
                    { current | stickies = stickyData.before ++ newSticky :: stickyData.after }
            in
            ( Viewing (categoryData.before ++ newCurrent :: categoryData.after), Cmd.none )

        ( SelectStickyColor _, _ ) ->
            ( model, Cmd.none )

        -- Editing Sticky content
        ( SetEditStickyMode category sticky, Viewing categories ) ->
            let
                cs =
                    getSpliCategories categories category.id

                categoryData =
                    EditingCategoryData cs.before cs.after cs.current cs.current

                ss =
                    getSplitStickies category.stickies sticky.id

                stickyData =
                    EditingStickyData ss.before ss.after ss.current ss.current
            in
            ( EditingStickyContent categoryData stickyData, Task.attempt (\_ -> NoOp) (Browser.Dom.focus "currentSticky") )

        ( SetEditStickyMode _ _, _ ) ->
            ( model, Cmd.none )

        ( EditStickyContent content, EditingStickyContent categoryData stickyData ) ->
            let
                sticky =
                    stickyData.current

                newSticky =
                    { sticky | content = content }

                newCategories =
                    Helpers.updateCurrentCategory categoryData newSticky
            in
            ( EditingStickyContent newCategories { stickyData | current = newSticky }, Cmd.none )

        ( EditStickyContent _, _ ) ->
            ( model, Cmd.none )

        ( SaveStickyContent, EditingStickyContent categoryData stickyData ) ->
            let
                current =
                    categoryData.current

                newCurrent =
                    { current | stickies = stickyData.before ++ stickyData.current :: stickyData.after }
            in
            ( Viewing (categoryData.before ++ newCurrent :: categoryData.after), Cmd.none )

        ( SaveStickyContent, _ ) ->
            ( model, Cmd.none )

        ( CancelStickyContent, EditingStickyContent categoryData stickyData ) ->
            let
                current =
                    categoryData.current

                newCurrent =
                    { current | stickies = stickyData.before ++ stickyData.initial :: stickyData.after }
            in
            ( Viewing (categoryData.before ++ newCurrent :: categoryData.after), Cmd.none )

        ( CancelStickyContent, _ ) ->
            ( model, Cmd.none )

        -- Delete Sticky
        ( SetDeleteStickyMode category sticky, Viewing categories ) ->
            let
                cs =
                    getSpliCategories categories category.id

                categoryData =
                    EditingCategoryData cs.before cs.after cs.current cs.current

                ss =
                    getSplitStickies category.stickies sticky.id

                stickyData =
                    EditingStickyData ss.before ss.after ss.current ss.current
            in
            ( DeletingSticky categoryData stickyData, Cmd.none )

        ( SetDeleteStickyMode _ _, _ ) ->
            ( model, Cmd.none )

        ( ValidateStickyDeletion, DeletingSticky categoryData stickyData ) ->
            let
                current =
                    categoryData.current

                newCurrent =
                    { current | stickies = stickyData.before ++ stickyData.after }

                categoryList =
                    categoryData.before ++ (newCurrent :: categoryData.after)
            in
            ( Viewing categoryList, Cmd.none )

        ( ValidateStickyDeletion, _ ) ->
            ( model, Cmd.none )

        ( CancelStickyDeletion, DeletingSticky categoryData _ ) ->
            let
                categoryList =
                    categoryData.before ++ (categoryData.current :: categoryData.after)
            in
            ( Viewing categoryList, Cmd.none )

        ( CancelStickyDeletion, _ ) ->
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
                    mainView True <| viewNormal categories

                EditingCategory data ->
                    mainView False <| viewEditingCategory data.before data.current data.after

                DeletingCategory data ->
                    mainView False <| viewDeletingCategory data.before data.current data.after

                EditingStickyColor categoryData stickyData ->
                    mainView False <| viewChangeStickyColor categoryData stickyData

                EditingStickyContent categoryData stickyData ->
                    mainView False <| viewEditStickyContent categoryData stickyData

                DeletingSticky categoryData stickyData ->
                    mainView False <| viewDeletingSticky categoryData stickyData
    in
    { title = title, body = [ body ] }


mainView : Bool -> Html Msg -> Html Msg
mainView active page =
    div
        [ class "w-full"
        , class "flex flex-col"
        , class "items-stretch"
        ]
        [ noteHeader
        , createCategory active
        , page
        ]


viewNormal : List Category -> Html Msg
viewNormal categories =
    div [] (List.map (viewCategory True) categories)


viewEditingCategory : List Category -> Category -> List Category -> Html Msg
viewEditingCategory before current after =
    let
        b =
            List.map (viewCategory False) before

        a =
            List.map (viewCategory False) after

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
            List.map (viewCategory False) before

        a =
            List.map (viewCategory False) after

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


createCategory : Bool -> Html Msg
createCategory active =
    div
        [ class "flex flex-row items-center"
        , class "fixed bottom-0 right-0"
        , class "mb-16 mr-16"
        ]
        [ Html.button
            [ class "flex flex-row items-center h-8"
            , class "p-2 px-4"
            , class "h-12"
            , class "rounded-full"
            , class "bg-blue-300"
            , class "font-bold text-white uppercase"
            , class "shadow-lg hover:shadow-xl"
            , if active then
                class "cursor-pointer"

              else
                class "cursor-not-allowed"
            , onClick CreateCategory
            ]
            [ Icons.plus [ class "pr-2" ]
            , span [] [ text "Category" ]
            ]
        ]



--- View category


viewCategory : Bool -> Category -> Html Msg
viewCategory active category =
    div
        [ class "w-full"
        , class "my-4"
        , class "bg-blue-100"
        , class "flex flex-col"
        ]
        [ viewCategoryHeader active category
        , if category.open then
            viewStickies active category category.stickies

          else
            div [] []
        ]


viewCategoryHeader : Bool -> Category -> Html Msg
viewCategoryHeader active category =
    let
        categoryTitle =
            if category.name == "" then
                Category.placeholder

            else
                category.name

        toggleCategory =
            if category.open then
                Icons.shrink
                    [ class "w-8 m-2 mx-4 text-gray-400"
                    , if active then
                        class "cursor-pointer"

                      else
                        class "cursor-not-allowed"
                    , onClick <| ToggleCategory category.id
                    ]

            else
                Icons.expand
                    [ class "w-8 m-2 mx-4 text-gray-400"
                    , if active then
                        class "cursor-pointer"

                      else
                        class "cursor-not-allowed"
                    , onClick <| ToggleCategory category.id
                    ]

        addIcon =
            if category.open then
                Icons.add
                    [ class "w-8 m-2"
                    , if active then
                        class "cursor-pointer"

                      else
                        class "cursor-not-allowed"
                    , onClick <| AddNote category
                    ]

            else
                span [] []
    in
    div
        [ class "flex flex-row"
        , class "items-center"
        , class "px-4 py-2"
        ]
        [ div
            [ class "uppercase h-100"
            , class "text-3xl font-bold text-gray-600"
            , onClick <| SetEditMode category.id
            ]
            [ text categoryTitle ]
        , toggleCategory
        , div [ class "flex-grow" ] []
        , div
            [ class "flex flex-row"
            , class "items-center"
            ]
            [ addIcon
            , Icons.delete
                [ class "w-8 m-2"
                , if active then
                    class "cursor-pointer"

                  else
                    class "cursor-not-allowed"
                , onClick <| SetDeleteCategoryMode category.id
                ]
            ]
        ]


viewStickies : Bool -> Category -> List Sticky -> Html Msg
viewStickies active category stickies =
    div
        [ class "flex flex-wrap flow-row"
        ]
        (List.map (viewSticky active category) stickies)


viewSticky : Bool -> Category -> Sticky -> Html Msg
viewSticky active category sticky =
    let
        stickyText =
            if sticky.content == "" then
                Sticky.placeholder

            else
                sticky.content
    in
    div
        [ class "w-64 h-64 p-4 m-4 min-w-64 min-h-64"
        , class "flex flex-col"
        , class "shadow"
        , Sticky.getColorAttribute sticky.color
        ]
        [ div
            [ class "flex flex-row h-10 pb-2" ]
            [ span [ class "flex-grow" ] []
            , Icons.color
                [ class "w-6 mx-1"
                , if active then
                    class "cursor-pointer"

                  else
                    class "cursor-not-allowed"
                , onClick (SetChangeColorMode category sticky)
                ]
            , Icons.delete
                [ class "w-6"
                , if active then
                    class "cursor-pointer"

                  else
                    class "cursor-not-allowed"
                , onClick (SetDeleteStickyMode category sticky)
                ]
            ]
        , div
            [ class "break-all"
            , class "overflow-y-auto whitespace-pre-wrap"
            , onClick (SetEditStickyMode category sticky)
            ]
            [ text stickyText ]
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
        , viewStickies False category category.stickies
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
            , Html.Attributes.placeholder Category.placeholder
            , Html.Attributes.id "currentCategory"
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
                , deleteButton True "Delete" ConfirmCategoryDeletion
                , undoButton True "Cancel" UndoCategoryDeletion
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
        , viewStickies False category category.stickies
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



--- Change sticky color


viewChangeStickyColor : EditingCategoryData -> EditingStickyData -> Html Msg
viewChangeStickyColor catData stickyData =
    let
        before =
            List.map (viewCategory False) catData.before

        current =
            viewChangeStickyColorCategory catData.current stickyData

        after =
            List.map (viewCategory False) catData.after
    in
    div [] (before ++ current :: after)


viewChangeStickyColorCategory : Category -> EditingStickyData -> Html Msg
viewChangeStickyColorCategory category stickyData =
    let
        before =
            List.map (viewSticky False category) stickyData.before

        after =
            List.map (viewSticky False category) stickyData.after

        current =
            viewChangeStickyColorSticky stickyData.current

        stickyList =
            div
                [ class "flex flex-wrap flow-row"
                ]
                (before ++ current :: after)
    in
    div
        [ class "w-full"
        , class "my-4"
        , class "bg-blue-100"
        , class "flex flex-col"
        ]
        [ viewCategoryHeader False category
        , stickyList
        ]


viewChangeStickyColorSticky : Sticky -> Html Msg
viewChangeStickyColorSticky sticky =
    div
        [ class "w-64 h-64 p-4 m-4 min-w-64 min-h-64"
        , class "flex flex-col"
        , class "shadow"
        , Sticky.getColorAttribute sticky.color
        ]
        [ div
            [ class "flex flex-row h-10 pb-2"
            ]
            [ span [ class "flex-grow" ] []
            , viewColorChips
            ]
        , div
            [ class "break-all"
            , class "overflow-y-auto whitespace-pre-wrap"
            ]
            [ text sticky.content ]
        ]


viewColorChips : Html Msg
viewColorChips =
    div
        [ class "flex flex-row"
        , class "p-2"
        , class "bg-gray-100"
        , class "rounded-full"
        , class "shadow"
        ]
        [ Sticky.colorChip Sticky.Yellow <| SelectStickyColor Sticky.Yellow
        , Sticky.colorChip Sticky.Peach <| SelectStickyColor Sticky.Peach
        , Sticky.colorChip Sticky.Pink <| SelectStickyColor Sticky.Pink
        , Sticky.colorChip Sticky.Purple <| SelectStickyColor Sticky.Purple
        , Sticky.colorChip Sticky.Blue <| SelectStickyColor Sticky.Blue
        , Sticky.colorChip Sticky.Teal <| SelectStickyColor Sticky.Teal
        , Sticky.colorChip Sticky.Green <| SelectStickyColor Sticky.Green
        ]



--- Editing Sticky content


viewEditStickyContent : EditingCategoryData -> EditingStickyData -> Html Msg
viewEditStickyContent catData stickyData =
    let
        before =
            List.map (viewCategory False) catData.before

        current =
            viewEditStickyContentCategory catData.current stickyData

        after =
            List.map (viewCategory False) catData.after
    in
    div [] (before ++ current :: after)


viewEditStickyContentCategory : Category -> EditingStickyData -> Html Msg
viewEditStickyContentCategory category stickyData =
    let
        before =
            List.map (viewSticky False category) stickyData.before

        after =
            List.map (viewSticky False category) stickyData.after

        current =
            viewEditSticky stickyData.current

        stickyList =
            div
                [ class "flex flex-wrap flow-row"
                ]
                (before ++ current :: after)
    in
    div
        [ class "w-full"
        , class "my-4"
        , class "bg-blue-100"
        , class "flex flex-col"
        ]
        [ viewCategoryHeader False category
        , stickyList
        ]


viewEditSticky : Sticky -> Html Msg
viewEditSticky sticky =
    div
        [ class "w-64 h-64 p-4 m-4 min-w-64 min-h-64"
        , class "flex flex-col"
        , class "shadow-outline"
        , Sticky.getColorAttribute sticky.color
        ]
        [ div
            [ class "flex flex-row h-10 pb-2"
            ]
            [ span [ class "flex-grow" ] []
            , Icons.validate
                [ class "w-6 mx-1 cursor-pointer"
                , onClick SaveStickyContent
                ]
            , Icons.cancel
                [ class "w-6 mx-1 cursor-pointer"
                , onClick CancelStickyContent
                ]
            ]
        , div
            [ class "break-all"
            , class "flex-grow"
            ]
            [ textarea
                [ class "w-full h-full"
                , class "resize-none"
                , Html.Attributes.id "currentSticky"
                , Sticky.getColorAttribute sticky.color
                , Html.Attributes.placeholder Sticky.placeholder
                , onInput EditStickyContent
                ]
                [ text sticky.content
                ]
            ]
        ]



--- Deleting Sticky


viewDeletingSticky : EditingCategoryData -> EditingStickyData -> Html Msg
viewDeletingSticky catData stickyData =
    let
        before =
            List.map (viewCategory False) catData.before

        current =
            viewDeleteStickyCategory catData.current stickyData

        after =
            List.map (viewCategory False) catData.after
    in
    div [] (before ++ current :: after)


viewDeleteStickyCategory : Category -> EditingStickyData -> Html Msg
viewDeleteStickyCategory category stickyData =
    let
        before =
            List.map (viewSticky False category) stickyData.before

        after =
            List.map (viewSticky False category) stickyData.after

        current =
            viewDeleteSticky stickyData.current

        stickyList =
            div
                [ class "flex flex-wrap flow-row"
                ]
                (before ++ current :: after)
    in
    div
        [ class "w-full"
        , class "my-4"
        , class "bg-blue-100"
        , class "flex flex-col"
        ]
        [ viewCategoryHeader False category
        , stickyList
        ]


viewDeleteSticky : Sticky -> Html Msg
viewDeleteSticky sticky =
    div
        [ class "w-64 h-64 p-4 m-4 min-w-64 min-h-64"
        , class "flex flex-col"
        , class "shadow-outline"
        , class "bg-red-400"
        ]
        [ div
            [ class "flex flex-row items-center h-10 mb-2"
            ]
            [ span
                [ class "font-extrabold text-white"
                , class "uppercase"
                ]
                [ text "Delete Sticky ?" ]
            , span [ class "flex-grow" ] []
            , Icons.validate
                [ class "w-6 mx-1 cursor-pointer"
                , onClick ValidateStickyDeletion
                ]
            , Icons.cancel
                [ class "w-6 mx-1 cursor-pointer"
                , onClick CancelStickyDeletion
                ]
            ]
        , div
            [ class "break-all"
            , class "overflow-y-auto whitespace-pre-wrap"
            , class "text-gray-800"
            ]
            [ text sticky.content ]
        ]



-- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
