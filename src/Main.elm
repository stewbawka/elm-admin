module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import CreateUser as CU
import DateFormat
import Html exposing (Html, a, button, div, h1, img, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, src)
import Http
import Iso8601 exposing (decoder)
import Json.Decode exposing (Decoder, andThen, at, bool, fail, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Routes
import Time
import UpdateUser as UU
import Url exposing (Url)
import Users as U
import WebSocket



---- MODEL ----


type Page
    = Users U.Model
    | CreateUser CU.Model
    | UpdateUser UU.Model
    | NotFound


type alias Model =
    { page : Page
    , navigationKey : Navigation.Key
    }


initialModel : Navigation.Key -> Model
initialModel navigationKey =
    { page = NotFound
    , navigationKey = navigationKey
    }


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init () url navigationKey =
    setNewPage (Routes.match url) (initialModel navigationKey)


setNewPage : Maybe Routes.Route -> Model -> ( Model, Cmd Msg )
setNewPage maybeRoute model =
    case maybeRoute of
        Just Routes.UsersList ->
            let
                ( usersModel, usersCmd ) =
                    U.init
            in
            ( { model | page = Users usersModel }
            , Cmd.map UsersMsg usersCmd
            )

        Just Routes.NewUser ->
            let
                ( createUserModel, createUserCmd ) =
                    CU.init model.navigationKey
            in
            ( { model | page = CreateUser createUserModel }
            , Cmd.map CreateUserMsg createUserCmd
            )

        Just (Routes.UpdateUser userId) ->
            let
                ( updateUserModel, updateUserCmd ) =
                    UU.init { userId = userId }
            in
            ( { model | page = UpdateUser updateUserModel }
            , Cmd.map UpdateUserMsg updateUserCmd
            )

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )



---- UPDATE ----


type Msg
    = NewRoute (Maybe Routes.Route)
    | Visit UrlRequest
    | UsersMsg U.Msg
    | CreateUserMsg CU.Msg
    | UpdateUserMsg UU.Msg


processPageUpdate :
    (pageModel -> Page)
    -> (pageMsg -> Msg)
    -> Model
    -> ( pageModel, Cmd pageMsg )
    -> ( Model, Cmd Msg )
processPageUpdate createPage wrapMsg model ( pageModel, pageCmd ) =
    ( { model | page = createPage pageModel }
    , Cmd.map wrapMsg pageCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( NewRoute maybeRoute, _ ) ->
            let
                ( updatedModel, cmd ) =
                    setNewPage maybeRoute model
            in
            ( updatedModel
            , Cmd.batch [ cmd, WebSocket.close () ]
            )

        ( UsersMsg usersMsg, Users usersModel ) ->
            let
                ( updatedUsersModel, usersCmd ) =
                    U.update usersMsg usersModel
            in
            ( { model | page = Users updatedUsersModel }
            , Cmd.map UsersMsg usersCmd
            )

        ( CreateUserMsg createUserMsg, CreateUser createUserModel ) ->
            let
                ( updatedCreateUserModel, createUserCmd ) =
                    CU.update createUserMsg createUserModel
            in
            ( { model | page = CreateUser updatedCreateUserModel }
            , Cmd.map CreateUserMsg createUserCmd
            )

        ( UpdateUserMsg updateUserMsg, UpdateUser updateUserModel ) ->
            let
                ( updatedUpdateUserModel, updateUserCmd ) =
                    UU.update updateUserMsg updateUserModel
            in
            ( { model | page = UpdateUser updatedUpdateUserModel }
            , Cmd.map UpdateUserMsg updateUserCmd
            )

        ( Visit (Browser.Internal url), _ ) ->
            ( model, Navigation.pushUrl model.navigationKey (Url.toString url) )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    let
        header =
            div [ class "nes-container is-centered is-rounded" ]
                [ a
                    [ class "nes-btn"
                    , href (Routes.routeToUrl Routes.UsersList)
                    ]
                    [ text "Users" ]
                ]

        content =
            case model.page of
                NotFound ->
                    text "Not Found"

                Users usersModel ->
                    U.view usersModel |> Html.map UsersMsg

                CreateUser createUserModel ->
                    CU.view createUserModel |> Html.map CreateUserMsg

                UpdateUser updateUserModel ->
                    UU.view updateUserModel |> Html.map UpdateUserMsg
    in
    { title = "Admin"
    , body = [ header, content ]
    }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = Visit
        , onUrlChange = Routes.match >> NewRoute
        }
