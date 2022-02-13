module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Element as E exposing (Element)
import Element.Background as Background
import Element.Font as Font


type alias View msg =
    { title : String
    , element : Element msg
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , element = E.text str
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , element = E.map fn view.element
    }


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument view =
    { title = view.title
    , body =
        [ E.layout
            [ Background.color (E.rgb255 42 44 43)
            , Font.color (E.rgb255 217 203 158)
            , E.paddingEach { top = 10, right = 0, bottom = 100, left = 0 }
            ]
            view.element
        ]
    }
