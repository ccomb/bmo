module Route exposing (Route(..), parseUrl, routeParser)

import Url
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, s, string, top)
import Url.Parser.Query as Query


type Route
    = NotFound
    | Optimizer


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Optimizer top
        ]


parseUrl : Url.Url -> Route
parseUrl url =
    Url.Parser.parse routeParser url
        |> Maybe.withDefault NotFound
