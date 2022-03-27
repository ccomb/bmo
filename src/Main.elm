module Main exposing (main)

import Browser exposing (Document)
import Element as E
import Pages.App as App
import Pages.Home as Home
import Route
import Shared
import Spa
import View exposing (View)


mappers : ( (a -> b) -> View a -> View b, (c -> d) -> View c -> View d )
mappers =
    ( View.map, View.map )


toDocument :
    Shared.Model
    -> View (Spa.Msg Shared.Msg pageMsg)
    -> Document (Spa.Msg Shared.Msg pageMsg)
toDocument shared view =
    { title = view.title
    , body =
        [ E.layout view.attributes
            view.element
        ]
    }


main =
    Spa.init
        { defaultView = View.defaultView
        , extractIdentity = Shared.identity
        }
        |> Spa.addPublicPage mappers Route.matchContent Home.page
        |> Spa.addPublicPage mappers Route.matchOptimizer App.page
        |> Spa.application View.map
            { toRoute = Route.toRoute
            , init = Shared.init
            , update = Shared.update
            , subscriptions = Shared.subscriptions
            , toDocument = toDocument
            , protectPage = Route.toUrl
            }
        |> Browser.application
