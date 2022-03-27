module Pages.Home exposing (Model, Msg, page)

import MarkupPage
import Shared
import Spa.Page
import View exposing (View)


type alias Msg =
    MarkupPage.Msg


type alias Model =
    MarkupPage.Model


page : Shared.Model -> Spa.Page.Page MarkupPage.Flags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = MarkupPage.init
        , update = MarkupPage.update
        , view = MarkupPage.view shared
        , subscriptions = MarkupPage.subscriptions
        }
