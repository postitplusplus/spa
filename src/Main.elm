module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Button exposing (button)
import Category
    exposing
        ( Category
        , emptyCategory
        )
import Html exposing (Html, div, header, input, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Icons
import Json.Decode as D
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


type Model
    = Viewing (List Category)
    | EditingCategory EditingCategoryData


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
      --
    | CreateCategory
    | AddNote
      -- Edit Category name
    | SetEditMode Int
    | EditCategoryName String
    | SaveCategoryName
    | CancelCategoryName


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            performUrlRequest urlRequest model

        ( ChangedUrl _, _ ) ->
            ( model, Cmd.none )

        ( CreateCategory, Viewing categories ) ->
            ( Viewing (categories ++ [ emptyCategory (List.length categories) ]), Cmd.none )

        ( CreateCategory, EditingCategory _ ) ->
            ( model, Cmd.none )

        ( AddNote, Viewing _ ) ->
            ( model, Cmd.none )

        ( AddNote, EditingCategory _ ) ->
            ( model, Cmd.none )

        ( SetEditMode id, Viewing categories ) ->
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

                data =
                    EditingCategoryData before after current current
            in
            ( EditingCategory data, Cmd.none )

        ( SetEditMode _, EditingCategory _ ) ->
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


noteHeader : Html Msg
noteHeader =
    header
        [ class "h-24"
        , class "bg-blue-300"
        , class "text-6xl font-bold text-center text-gray-200 uppercase"
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
    let
        postit =
            div [] []
    in
    div
        [ class "w-full"
        , class "my-4"
        , class "bg-blue-100"
        , class "flex flex-col"
        ]
        [ viewCategoryHeader category
        , postit
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
            [ span [ class "m-2 mr-6" ] [ button "Add note" AddNote ]
            , span
                [ class "m-2 cursor-pointer"
                , onClick <| SetEditMode category.id
                ]
                [ Icons.edit ]
            , span [ class "m-2" ] [ Icons.delete ]
            ]
        ]



--- Edit category


viewEditCategory : Category -> Html Msg
viewEditCategory category =
    let
        postit =
            div [] []
    in
    div
        [ class "w-full"
        , class "my-4"
        , class "bg-blue-300"
        , class "flex flex-col"
        ]
        [ viewEditCategoryHeader category
        , postit
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
            [ span
                [ class "m-2 cursor-pointer"
                , onClick SaveCategoryName
                ]
                [ Icons.validate ]
            , span
                [ class "m-2 cursor-pointer"
                , onClick CancelCategoryName
                ]
                [ Icons.cancel ]
            ]
        ]



-- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
