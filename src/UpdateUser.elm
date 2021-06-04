module UpdateUser exposing (Model, Msg, init, update, view)

import Users as U exposing (User, userDecoder)
import Html exposing (Html, a, button, form, input, label, text, div, h1, img)
import Html.Attributes exposing (class, for, href, id, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, at)
import Json.Encode as Encode
import Routes

type alias Model =
    { error : Maybe Http.Error
    , user : Maybe User
    }

baseUrl : String
baseUrl =
    "http://localhost:8080"

initialModel : Model
initialModel =
    { user = Nothing
    , error = Nothing
    }

saveUser : User -> Cmd Msg
saveUser user =
    let
        body =
            Encode.object
                [ ("first_name", Encode.string user.firstName)
                , ("last_name", Encode.string user.lastName)
                , ("email", Encode.string user.email)
                ] |> Http.jsonBody
    in
    Http.request
        { url = baseUrl ++ "/users/" ++ (String.fromInt user.id)
        , method = "PATCH"
        , body = body
        , expect = Http.expectJson LoadUser U.userDecoder
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }

userDetailDecoder : Decoder User
userDetailDecoder =
    at ["data"] U.userDecoder

fetchUser : Int -> Cmd Msg
fetchUser userId =
    Http.get
        { url = baseUrl ++ "/users/" ++ (String.fromInt userId)
        , expect = Http.expectJson LoadUser userDetailDecoder
        }

init : { userId : Int } -> ( Model, Cmd Msg )
init { userId  } =
    ( initialModel, fetchUser userId)


type Msg
    = SaveUser
    | LoadUser (Result Http.Error User)
    | UpdateField String String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveUser ->
            case model.user of
                Just user ->
                    ( model, saveUser user )
                Nothing ->
                    ( model, Cmd.none )
        LoadUser (Ok user) ->
            ( { model | user = Just user }, Cmd.none )
        LoadUser (Err error) ->
            ( { model | error = Just error }, Cmd.none )
        UpdateField fieldName newValue ->
            case model.user of
                Just user ->
                    let 
                        oldUser = user
                        updatedUser =
                            case fieldName of 
                                "firstName" -> 
                                    { oldUser | firstName = newValue }
                                "lastName" -> 
                                    { oldUser | lastName = newValue }
                                "email" -> 
                                    { oldUser | email = newValue }
                                _ ->
                                    oldUser
                    in
                    ( { model | user = Just updatedUser }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

viewUserForm : User -> Html Msg
viewUserForm user =
    div [ class "nes-container" ]
        [ form []
               [ div [ class "nes-field" ]
                     [ label [ for "firstName" ]
                             [ text "First Name" ]
                     , input [ type_ "text"
                             , id "firstName"
                             , value user.firstName
                             , class "nes-input"
                             , onInput (UpdateField "firstName")
                             ]
                             []
                     ]
               , div [ class "nes-field" ]
                     [ label [ for "lastName" ]
                             [ text "Last Name" ]
                     , input [ type_ "text"
                             , id "lastName"
                             , value user.lastName
                             , class "nes-input"
                             , onInput (UpdateField "lastName")
                             ]
                             []
                     ]
               , div [ class "nes-field" ]
                     [ label [ for "email" ]
                             [ text "Email" ]
                     , input [ type_ "email"
                             , id "email"
                             , value user.email
                             , class "nes-input"
                             , onInput (UpdateField "email")
                             ]
                             []
                     ]
               , button [ type_ "button"
                        , class "nes-btn is-primary"
                        , onClick SaveUser
                        ]
                        [ text "Save" ]
               ]
        ]

view : Model -> Html Msg
view model =
    let
        content =
            case model.user of
                Just user ->
                    viewUserForm user
                Nothing ->
                    case model.error of
                        Just error ->
                            text (Debug.toString error)
                        Nothing ->
                            text "loading.."
    in
    div [ class "content" ]
        [ content
        , div [ class "nes-container is-centered" ]
              [ a [ class "nes-btn is-primary" 
                  , href (Routes.routeToUrl Routes.UsersList)
                  ] 

                  [ text "Users List" ]
              ]
        ]
