module Routes exposing (Route(..), href, match)

import Html
import Html.Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)

type Route
    = UsersList
    | NewUser

match : Url -> Maybe Route
match url =
    Parser.parse routes url

routeToUrl : Route -> String
routeToUrl route =
    case route of
        UsersList ->
            "/users"

        NewUser ->
            "/users/new"

href : Route -> Html.Attribute msg
href route =
    Html.Attributes.href (routeToUrl route)
    
routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map UsersList (Parser.s "users")
        , Parser.map NewUser (Parser.s "users" </> Parser.s "new")
        ]
