module Routes exposing (Route(..), href, match, replaceUrl, routeToUrl)

import Browser.Navigation as Nav
import Html
import Html.Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = UsersList
    | NewUser
    | UpdateUser Int


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

        UpdateUser userId ->
            "/users/" ++ String.fromInt userId


href : Route -> Html.Attribute msg
href route =
    Html.Attributes.href (routeToUrl route)


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map UsersList (Parser.s "users")
        , Parser.map NewUser (Parser.s "users" </> Parser.s "new")
        , Parser.map UpdateUser (Parser.s "users" </> Parser.int)
        ]


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToUrl route)
