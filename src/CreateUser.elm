module CreateUser exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Users as U exposing (User, userDecoder)
import Html exposing (Html, a, button, form, input, label, text, div, h1, img)
import Html.Attributes exposing (class, for, href, id, src, type_)
import Html.Events exposing (onClick, onInput)
import Http exposing (Metadata, expectStringResponse)
import Json.Decode exposing (Decoder, at, decodeString, errorToString, map, string, succeed)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Routes exposing (replaceUrl)

type alias Model =
    { error : Maybe String
    , userErrors : NewUserErrors
    , newUser : NewUser
    , navKey : Navigation.Key
    }

type alias NewUser =
    { firstName : String
    , lastName : String
    , email : String
    }

type Error body
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Metadata body Int
    | BadBody Metadata body String


type alias NewUserErrors =
    { firstName : Maybe String
    , lastName : Maybe String
    , email : Maybe String
    }

errorDecoder : Decoder NewUserErrors
errorDecoder =
    succeed NewUserErrors
        |> optional "first_name" (map Just string) Nothing
        |> optional "last_name" (map Just string) Nothing
        |> optional "email" (map Just string) Nothing

userErrorsDecoder : Decoder NewUserErrors
userErrorsDecoder =
    at ["errors"] errorDecoder


baseUrl : String
baseUrl =
    "http://localhost:8080"

initialNewUser =
    { firstName = ""
    , lastName = ""
    , email = ""
    }

initialNewUserErrors =
    { firstName = Nothing
    , lastName = Nothing
    , email = Nothing
    }

initialModel : Navigation.Key -> Model
initialModel  navKey =
    { newUser = initialNewUser
    , error = Nothing
    , userErrors = initialNewUserErrors
    , navKey = navKey
    }

userDetailDecoder : Decoder User
userDetailDecoder =
    at ["data"] U.userDecoder

expectJson : (Result (Error String) a -> msg) -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
  expectStringResponse toMsg <|
    \response ->
      case response of
        Http.BadUrl_ url ->
          Err (BadUrl url)

        Http.Timeout_ ->
          Err Timeout

        Http.NetworkError_ ->
          Err NetworkError

        Http.BadStatus_ metadata body ->
          Err (BadStatus metadata body metadata.statusCode)

        Http.GoodStatus_ metadata body ->
          case decodeString decoder body of
            Ok value ->
              Ok value

            Err err ->
              Err (BadBody metadata body (errorToString err))

saveUser : NewUser -> Cmd Msg
saveUser newUser =
    let
        body =
            Encode.object
                [ ("first_name", Encode.string newUser.firstName)
                , ("last_name", Encode.string newUser.lastName)
                , ("email", Encode.string newUser.email)
                ] |> Http.jsonBody
    in
    Http.post
        { url = baseUrl ++ "/users"
        , body = body
        , expect = expectJson LoadSavedUser userDetailDecoder
        }

init : Navigation.Key -> ( Model, Cmd Msg )
init navKey =
    ( initialModel navKey, Cmd.none )

type Msg
    = SaveUser
    | LoadSavedUser (Result (Error String) U.User)
    | UpdateField String String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveUser ->
            ( model, saveUser model.newUser )
        LoadSavedUser (Ok createdUser) ->
            ( model, replaceUrl model.navKey (Routes.UpdateUser createdUser.id))
        LoadSavedUser (Err errorMsg) ->
            case errorMsg of
                BadStatus _ body _ ->
                    case decodeString userErrorsDecoder body of
                        Ok value ->
                            ( { model | userErrors = value }, Cmd.none )
                        Err err ->
                            ( { model | error = Just (errorToString err) }, Cmd.none )
                _ ->
                    ( { model | error = Just (Debug.toString errorMsg) }, Cmd.none )



        UpdateField fieldName newValue ->
            let 
                oldNewUser = model.newUser
                updatedNewUser =
                    case fieldName of 
                        "firstName" -> 
                            { oldNewUser | firstName = newValue }
                        "lastName" -> 
                            { oldNewUser | lastName = newValue }
                        "email" -> 
                            { oldNewUser | email = newValue }
                        _ ->
                            oldNewUser
            in
            ( { model | newUser = updatedNewUser }, Cmd.none )

viewModel : Model -> Html Msg
viewModel model =
    text (Debug.toString model)


viewUserForm : Model -> Html Msg
viewUserForm model =
    let
        firstNameClasses =
            case model.userErrors.firstName of
                Nothing -> "nes-input"
                _ -> "nes-input is-error"
        lastNameClasses =
            case model.userErrors.lastName of
                Nothing -> "nes-input"
                _ -> "nes-input is-error"
        emailClasses =
            case model.userErrors.email of
                Nothing -> "nes-input"
                _ -> "nes-input is-error"
    in
    div [ class "nes-container" ]
        [ form []
               [ div [ class "nes-field" ]
                     [ label [ for "firstName" ]
                             [ text "First Name" ]
                     , input [ type_ "text"
                             , id "firstName"
                             , class firstNameClasses
                             , onInput (UpdateField "firstName")
                             ]
                             []
                     ]
               , div [ class "nes-field" ]
                     [ label [ for "lastName" ]
                             [ text "Last Name" ]
                     , input [ type_ "text"
                             , id "lastName"
                             , class lastNameClasses
                             , onInput (UpdateField "lastName")
                             ]
                             []
                     ]
               , div [ class "nes-field" ]
                     [ label [ for "email" ]
                             [ text "Email" ]
                     , input [ type_ "email"
                             , id "email"
                             , class emailClasses
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
    div [ class "content" ]
        [ viewModel model
        , viewUserForm model
        , div [ class "nes-container is-centered" ]
              [ a [ class "nes-btn is-primary" 
                  , href (Routes.routeToUrl Routes.UsersList)
                  ] 

                  [ text "Users List" ]
              ]
        ]
