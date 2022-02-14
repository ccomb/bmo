module Pages.Home_ exposing (Model, Msg, page)

import Gen.Params.Home_ exposing (Params)
import MarkupPage
import Page
import Request
import Shared


type alias Msg =
    MarkupPage.Msg


type alias Model =
    MarkupPage.Model


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = MarkupPage.init req
        , update = MarkupPage.update
        , view = MarkupPage.view shared req
        , subscriptions = MarkupPage.subscriptions
        }
