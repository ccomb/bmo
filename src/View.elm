module View exposing (View, defaultView, map, none, placeholder, toBrowserDocument)

import Browser
import Element as E exposing (Element)


type alias View msg =
    { title : String
    , attributes : List (E.Attribute msg)
    , element : Element msg
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , attributes = []
    , element = E.text str
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , attributes = List.map (E.mapAttribute fn) view.attributes
    , element = E.map fn view.element
    }


defaultView : View msg
defaultView =
    placeholder "404"


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument view =
    { title = view.title
    , body =
        [ E.layout view.attributes
            view.element
        ]
    }
