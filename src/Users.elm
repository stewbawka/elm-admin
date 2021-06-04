module Users exposing (Model, Msg, User, init, update, userDecoder, view)

import Time
import Html exposing (Html, a, button, table, tbody, thead, text, td, th, tr, div, h1, img)
import Html.Attributes exposing (class, href, src)
import Http
import Json.Decode exposing (Decoder, andThen, at, bool, fail, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Iso8601 exposing (decoder)
import DateFormat
import Routes


---- MODEL ----

type alias Model =
    { error : Maybe Http.Error
    , users : Maybe UserList
    }

type alias User =
    { id: Int
    , firstName : String
    , lastName : String
    , email : String
    , createdAt : Time.Posix
    }

type alias UserList =
    List User

userDecoder : Decoder User
userDecoder =
    succeed User
        |> required "id" int
        |> required "first_name" string
        |> required "last_name" string
        |> required "email" string
        |> required "created_at" decoder

userListDecoder : Decoder (List User)
userListDecoder =
    at ["data"] (list userDecoder)

baseUrl : String
baseUrl =
    "http://localhost:8080"

initialModel : Model
initialModel  =
    { users = Nothing
    , error = Nothing
    }

fetchUsers : Cmd Msg
fetchUsers =
    Http.get
        { url = baseUrl ++ "/users"
        , expect = Http.expectJson LoadUsers userListDecoder
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchUsers )

---- UPDATE ----


type Msg
    = LoadUsers (Result Http.Error (List User))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadUsers (Ok userList) ->
            ( { model | users = Just userList }, Cmd.none )
        LoadUsers (Err _) ->
            ( model, Cmd.none )

---- VIEW ----


viewUserRow : User -> Html Msg
viewUserRow user =
    let
        createdAtStr =
            DateFormat.format "dddd, MMMM dd, yyyy" Time.utc user.createdAt
    in
        tr [] 
           [ td [] [ text (user.firstName ++ " " ++ user.lastName) ]
           , td [] [ text user.email ]
           , td [] [ text createdAtStr ]
           ]

viewUserList : Maybe (List User) -> Html Msg
viewUserList maybeUsers =
    case maybeUsers of
        Just userList ->
            div [ class "nes-table-responsive" ]
                [ table [ class "nes-table is-bordered " ]
                        [ thead []
                                [ tr [] 
                                     [ th [] [ text "Name" ]
                                     , th [] [ text "Email" ]
                                     , th [] [ text "Created At" ]
                                     ]
                                ]
                        , tbody [] (List.map viewUserRow userList)
                        ]
                ]
        Nothing ->
            div [ class "nes-container is-centered is-rounded" ]
                [ text "Loading users..." ]
            

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewUserList model.users
        , div [ class "nes-container is-centered" ]
              [ a [ class "nes-btn is-primary" 
                  , href (Routes.routeToUrl Routes.NewUser)
                  ] 


                  [ text "Add User" ]
              ]
        ]
