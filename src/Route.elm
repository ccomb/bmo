module Route exposing (OptimFlags(..), Route(..), formulaUrl, idUrl, matchContent, matchOptimizer, routeParser, toRoute, toUrl)

import Optim exposing (Formula)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser exposing ((<?>), Parser, map, oneOf, s, string, top)
import Url.Parser.Query as Query


type Route
    = NotFound Url
    | Home
    | Optimizer OptimFlags -- optimize from a formula or Id
    | Content String -- dynamic text page


type OptimFlags
    = OQFormula Formula
    | OQId String
    | OQEmpty


toOptimFlags : Maybe Formula -> Maybe String -> OptimFlags
toOptimFlags f i =
    case f of
        Just formula ->
            case i of
                Just id ->
                    OQId id

                Nothing ->
                    OQFormula formula

        Nothing ->
            case i of
                Just id ->
                    OQId id

                Nothing ->
                    OQEmpty


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Optimizer (s "app" <?> Query.map2 toOptimFlags (Query.string "formula") (Query.string "id"))
        , map Content string
        ]


formulaUrl : Formula -> String
formulaUrl f =
    Builder.absolute [ "app" ] [ Builder.string "formula" f ]


idUrl : String -> String
idUrl i =
    Builder.absolute [ "app" ] [ Builder.string "id" i ]


toUrl : Route -> String
toUrl r =
    case r of
        Home ->
            "/"

        Optimizer oq ->
            case oq of
                OQFormula f ->
                    formulaUrl f

                OQId i ->
                    idUrl i

                OQEmpty ->
                    ""

        Content url ->
            url

        NotFound url ->
            Url.toString url


toRoute : Url -> Route
toRoute url =
    url
        |> Url.Parser.parse routeParser
        |> Maybe.withDefault (NotFound url)


matchContent : Route -> Maybe String
matchContent r =
    case r of
        Home ->
            Just "home_"

        Content path ->
            Just path

        _ ->
            Nothing


matchOptimizer : Route -> Maybe OptimFlags
matchOptimizer r =
    case r of
        Optimizer oq ->
            Just oq

        _ ->
            Nothing
