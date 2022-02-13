module Pages.Home_ exposing (view)

import Element as E exposing (Element)
import View exposing (View)


view : View msg
view =
    { title = "Homepage"
    , element = E.el [] <| E.column [] [ E.text "Hello, world!", E.link [] { url = "app", label = E.text "App" } ]
    }
