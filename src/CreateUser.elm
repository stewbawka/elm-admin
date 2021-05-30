module CreateUser exposing (Model, Msg, init, update, view)

import Users as U exposing (User, userDecoder)
import Html exposing (Html, a, button, form, input, label, text, div, h1, img)
import Html.Attributes exposing (class, for, href, id, src, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode
import Routes

type alias Model =
    { error : Maybe Http.Error
    , newUser : NewUser
    }

type alias NewUser =
    { firstName : String
    , lastName : String
    , email : String
    }

baseUrl : String
baseUrl =
    "http://localhost:8080"

initialNewUser =
    { firstName = ""
    , lastName = ""
    , email = ""
    }

initialModel : Model
initialModel  =
    { newUser = initialNewUser
    , error = Nothing
    }

saveUser : NewUser -> Cmd Msg
saveUser newUser =
    let
        body =
            Encode.object
                [ ("first_name", Encode.string newUser.firstName)
                , ("last_name", Encode.string newUser.lastName)
                , ("email", Encode.string newUser.email)
                , ("password", Encode.string "test1234")
                ] |> Http.jsonBody
    in
    Http.post
        { url = baseUrl ++ "/users"
        , body = body
        , expect = Http.expectJson LoadSavedUser U.userDecoder
        }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

type Msg
    = SaveUser
    | LoadSavedUser (Result Http.Error U.User)
    | UpdateField String String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveUser ->
            ( model, saveUser model.newUser )
        LoadSavedUser (Ok createdUser) ->
            ( model, Cmd.none )
        LoadSavedUser (Err _) ->
            ( model, Cmd.none )
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



viewUserForm : NewUser -> Html Msg
viewUserForm newUser =
    div [ class "nes-container" ]
        [ form []
               [ div [ class "nes-field" ]
                     [ label [ for "firstName" ]
                             [ text "First Name" ]
                     , input [ type_ "text"
                             , id "firstName"
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
    div [ class "content" ]
        [ viewUserForm model.newUser
        , div [ class "nes-container is-centered" ]
              [ a [ class "nes-btn is-primary" 
                  , href (Routes.routeToUrl Routes.UsersList)
                  ] 

                  [ text "Users List" ]
              ]
        ]
