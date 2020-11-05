module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Category
    exposing
        ( Category
        , emptyCategory
        )
import Html exposing (Html, button, div, header, text)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            performUrlRequest urlRequest model

        ( ChangedUrl _, _ ) ->
            ( model, Cmd.none )

        ( CreateCategory, App categories ) ->
            ( App (emptyCategory :: categories), Cmd.none )


performUrlRequest : UrlRequest -> Model -> ( Model, Cmd msg )
performUrlRequest request model =
    case request of
        Internal _ ->
            ( model, Cmd.none )

        External url ->
            ( model, Nav.load url )



--- VIEW ---


view : Model -> Document Msg
view _ =
    let
        title =
            "Post it, plus plus"

        body =
            [ mainView
            ]
    in
    { title = title, body = body }


mainView : Html Msg
mainView =
    div
        [ class "w-full"
        , class "flex flex-col"
        , class "items-stretch"
        ]
        [ noteHeader
        , createCategory
        ]


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
        [ button
            [ class "px-6 py-2 rounded-full"
            , class "bg-blue-300"
            , class "font-bold text-white uppercase"
            , class "shadow-md hover:shadow-lg"
            , onClick CreateCategory
            ]
            [ text "Create category" ]
        ]



-- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
