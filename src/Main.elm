module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Button exposing (button)
import Category
    exposing
        ( Category
        , emptyCategory
        )
import Html exposing (Html, div, header, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
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


type Model
    = App (List Category)


init : D.Value -> Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( App [], Cmd.none )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            performUrlRequest urlRequest model

        ( ChangedUrl _, _ ) ->
            ( model, Cmd.none )

        ( CreateCategory, App categories ) ->
            ( App (emptyCategory :: categories), Cmd.none )

        ( AddNote, App _ ) ->
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
        categories =
            case model of
                App c ->
                    c

        title =
            "Post it, plus plus"

        body =
            [ mainView categories
            ]
    in
    { title = title, body = body }


mainView : List Category -> Html Msg
mainView categories =
    div
        [ class "w-full"
        , class "flex flex-col"
        , class "items-stretch"
        ]
        ([ noteHeader
         , createCategory
         ]
            ++ List.map viewCategory categories
        )


noteHeader : Html Msg
noteHeader =
    header
        [ class "h-24"
        , class "bg-blue-300"
        , class "text-6xl text-center text-white"
        ]
        [ text "My Notes" ]


createCategory : Html Msg
createCategory =
    div
        [ class "p-8" ]
        [ button "Create Category" CreateCategory
        ]


viewCategory : Category -> Html Msg
viewCategory category =
    let
        topBar =
            div
                [ class "flex flex-row"
                , class "px-4 py-2"
                ]
                [ div [] [ text category.name ]
                , div [ class "flex-grow" ] []
                , div
                    []
                    [ button "Add note" AddNote
                    , text "B"
                    , text "C"
                    ]
                ]

        postit =
            div [] []
    in
    div
        [ class "w-full"
        , class "my-4"
        , class "bg-blue-100"
        , class "flex flex-col"
        ]
        [ topBar
        , postit
        ]



-- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
