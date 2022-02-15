module MarkupPage exposing (Model, Msg, init, subscriptions, update, view)

import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes
import Http
import Mark
import Mark.Error
import Maybe
import Request exposing (Request)
import Result exposing (Result)
import Set exposing (Set)
import Shared exposing (WindowSize)
import View exposing (View)


type MarkupContent
    = Loading
    | Failed String
    | Loaded String


type alias Model =
    { markup : MarkupContent
    , openIFrames : Set String
    }


type Msg
    = GotMarkup (Result Http.Error String)
    | OpenedIFrame String


init : Request -> ( Model, Cmd Msg )
init req =
    ( { markup = Loading
      , openIFrames = Set.empty
      }
    , Http.get
        { url =
            let
                path =
                    if req.url.path == "/" then
                        "home_"

                    else
                        req.url.path
            in
            "/content/" ++ path
        , expect = Http.expectString GotMarkup
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMarkup result ->
            case result of
                Ok markup ->
                    ( { model
                        | markup = Loaded markup
                      }
                    , Cmd.none
                    )

                Err error ->
                    -- TODO
                    ( { model
                        | markup = Failed "Could not load the content"
                      }
                    , Cmd.none
                    )

        OpenedIFrame name ->
            ( { model | openIFrames = Set.insert name model.openIFrames }, Cmd.none )


view : Shared.Model -> Request -> Model -> View Msg
view shared _ model =
    case model.markup of
        Loading ->
            loadingPage

        Failed error ->
            errorPage error

        Loaded content ->
            markupview shared model content


loadingPage : View Msg
loadingPage =
    { title = "Loading"
    , attributes = []
    , element = E.el [ E.centerX, E.centerY ] <| E.image [] { src = "/public/bspinner.png", description = "spinner" }
    }


errorPage : String -> View Msg
errorPage error =
    { title = "Error"
    , attributes = []
    , element = E.text <| "Error: " ++ error
    }


markupview : Shared.Model -> Model -> String -> View Msg
markupview shared model content =
    case Mark.compile (document shared model) content of
        Mark.Success element ->
            { title = "Business Model Optimizer"
            , attributes = []
            , element = element
            }

        Mark.Almost { result, errors } ->
            { title = "BMO: Warning"
            , attributes = []
            , element =
                E.column [] <| viewErrors errors
            }

        Mark.Failure error ->
            { title = "BMO: Failure"
            , attributes = []
            , element =
                E.row
                    [ E.centerX
                    , E.centerY
                    , Background.color (E.rgb255 240 240 240)
                    , Border.rounded 10
                    , E.padding 50
                    , Border.shadow
                        { color = E.rgb255 200 200 200
                        , blur = 5
                        , offset = ( 3, 3 )
                        , size = 0.1
                        }
                    ]
                    (viewErrors error)
            }


viewErrors : List Mark.Error.Error -> List (Element msg)
viewErrors errors =
    List.map (E.html << Mark.Error.toHtml Mark.Error.Light) errors



--------
-- TOOLS
--------


paddingAuto : WindowSize -> Int -> E.Attribute msg
paddingAuto size =
    -- automatic padding for the content width
    E.paddingXY (size.w // 3 - 130)


decodeColor : String -> E.Color
decodeColor colorstring =
    let
        l =
            String.split " " colorstring
                |> List.map String.toFloat
                |> List.map (Maybe.withDefault 100)
                |> List.map ((*) 0.01)
    in
    E.rgba
        (l |> List.head |> Maybe.withDefault 100)
        (List.drop 1 l |> List.head |> Maybe.withDefault 100)
        (List.drop 2 l |> List.head |> Maybe.withDefault 100)
        (List.drop 3 l |> List.head |> Maybe.withDefault 100)



--------------------------------------
-- describe the accepted markup blocks
--------------------------------------


document : Shared.Model -> Model -> Mark.Document (E.Element Msg)
document shared model =
    let
        size =
            shared.windowSize
    in
    Mark.document
        (E.column
            [ E.width (E.px size.w)
            , Font.family
                [ Font.typeface "Gotu"
                ]
            , E.spacing 20
            ]
        )
        (Mark.manyOf
            [ hero size
            , title size 1
            , title size 2
            , title size 3
            , Mark.map (E.paragraph [ E.spacing 10, paddingAuto size 0 ]) textBlock
            , ctaBlock size
            , imageBlock size
            , iframeBlock model size
            , treeBlock size
            , footerBlock size
            ]
        )


hero : WindowSize -> Mark.Block (E.Element msg)
hero size =
    Mark.record "hero"
        (\backgroundColor textColor img tagline titleBackground url text background color heightRatio ->
            E.row
                [ E.height <| E.px <| floor <| toFloat size.h * heightRatio
                , Background.color (decodeColor backgroundColor)
                , Background.image img
                , Region.heading 1
                , Font.size (size.h // 30 |> min 40 |> max 25)
                , Font.extraBold
                , E.width E.fill
                ]
                [ E.column
                    [ E.centerX
                    , E.width (E.px (max 400 (size.w // 2)))
                    , Background.color <| decodeColor titleBackground
                    , Font.color (decodeColor textColor)
                    , Border.rounded 15
                    , E.padding 40
                    , E.spacing 30
                    ]
                    [ E.paragraph
                        []
                        [ E.text tagline
                        ]
                    , E.link
                        [ Background.color <| decodeColor background
                        , E.spacing 30
                        , Font.color <| decodeColor color
                        , E.paddingXY 20 10
                        , Border.rounded 10
                        , Border.shadow
                            { color = E.rgb255 20 20 20
                            , blur = 10
                            , offset = ( 3, 3 )
                            , size = 0.1
                            }
                        ]
                        { url = url
                        , label = E.paragraph [] [ E.text text ]
                        }
                    ]
                ]
        )
        |> Mark.field "backgroundColor" Mark.string
        |> Mark.field "textColor" Mark.string
        |> Mark.field "image" Mark.string
        |> Mark.field "title" Mark.string
        |> Mark.field "titleBackground" Mark.string
        |> Mark.field "url" Mark.string
        |> Mark.field "text" Mark.string
        |> Mark.field "background" Mark.string
        |> Mark.field "color" Mark.string
        |> Mark.field "heightRatio" Mark.float
        |> Mark.toBlock


imageBlock : WindowSize -> Mark.Block (E.Element msg)
imageBlock size =
    Mark.record "Image"
        (\src desc ->
            E.column [ E.centerX, paddingAuto size 0 ]
                [ E.image [] { src = src, description = desc }
                ]
        )
        |> Mark.field "src" Mark.string
        |> Mark.field "desc" Mark.string
        |> Mark.toBlock


iframeBlock : Model -> WindowSize -> Mark.Block (E.Element Msg)
iframeBlock model size =
    Mark.record "iframe"
        (\name button color bcolor src width height background ->
            E.el
                [ E.width E.fill
                , paddingAuto size 0
                , Background.color (decodeColor background)
                ]
            <|
                if Set.member name model.openIFrames then
                    E.html <|
                        Html.iframe
                            [ Html.Attributes.width width
                            , Html.Attributes.height height
                            , Html.Attributes.src src
                            , Html.Attributes.style "border" "none"
                            ]
                            []

                else
                    Input.button
                        [ Background.color <| decodeColor bcolor
                        , E.spacing 30
                        , Font.color <| decodeColor color
                        , E.paddingXY 20 10
                        , Border.rounded 10
                        , Border.shadow
                            { color = E.rgb255 20 20 20
                            , blur = 10
                            , offset = ( 3, 3 )
                            , size = 0.1
                            }
                        ]
                        { onPress = Just (OpenedIFrame name), label = E.text button }
        )
        |> Mark.field "name" Mark.string
        |> Mark.field "button" Mark.string
        |> Mark.field "color" Mark.string
        |> Mark.field "bcolor" Mark.string
        |> Mark.field "src" Mark.string
        |> Mark.field "width" Mark.int
        |> Mark.field "height" Mark.int
        |> Mark.field "background" Mark.string
        |> Mark.toBlock


link =
    Mark.annotation "link"
        (\styles url ->
            E.link
                [ Font.extraBold ]
                { label =
                    styles
                        |> List.map (\s -> E.el (fromMark (Tuple.first s)) (E.text <| Tuple.second s))
                        |> E.paragraph []
                , url = url
                }
        )
        |> Mark.field "url" Mark.string


cta =
    Mark.annotation "cta"
        (\styles url color background ->
            E.newTabLink
                [ Background.color (decodeColor background)
                , Font.color <| decodeColor color
                , E.paddingXY 5 0
                , Border.rounded 10
                , Border.shadow
                    { color = E.rgb255 20 20 20
                    , blur = 5
                    , offset = ( 2, 2 )
                    , size = 0.1
                    }
                ]
                { label =
                    styles
                        |> List.map (\s -> E.el (fromMark (Tuple.first s)) (E.text <| Tuple.second s))
                        |> E.paragraph []
                , url = url
                }
        )
        |> Mark.field "url" Mark.string
        |> Mark.field "color" Mark.string
        |> Mark.field "background" Mark.string


image =
    Mark.annotation "img"
        (\_ src width desc -> E.image [ E.width (E.px width) ] { src = src, description = desc })
        |> Mark.field "src" Mark.string
        |> Mark.field "width" Mark.int
        |> Mark.field "desc" Mark.string


fromMark : Mark.Styles -> List (E.Attribute msg)
fromMark styles =
    List.concat
        [ if styles.bold then
            [ Font.bold ]

          else
            []
        , if styles.italic then
            [ Font.italic ]

          else
            []
        , if styles.strike then
            [ Font.strike ]

          else
            []
        ]


textBlock : Mark.Block (List (E.Element msg))
textBlock =
    Mark.textWith
        { view = \styles str -> E.el (fromMark styles) (E.text str)
        , replacements = []
        , inlines = [ link, cta, image ]
        }


title : WindowSize -> Int -> Mark.Block (E.Element msg)
title size level =
    -- H1, H2, H3 title
    Mark.block ("H" ++ String.fromInt level)
        (\children ->
            E.paragraph
                [ Region.heading level
                , Font.size (45 - 6 * level)
                , Font.heavy
                , paddingAuto size 20
                ]
                children
        )
        textBlock


ctaBlock : WindowSize -> Mark.Block (E.Element msg)
ctaBlock size =
    Mark.record "ctaBlock"
        (\text url color background ->
            E.row [ paddingAuto size 0 ]
                [ E.link
                    [ Background.color <| decodeColor background
                    , E.spacing 30
                    , Font.color <| decodeColor color
                    , E.paddingXY 20 10
                    , Border.rounded 10
                    , Border.shadow
                        { color = E.rgb255 20 20 20
                        , blur = 10
                        , offset = ( 3, 3 )
                        , size = 0.1
                        }
                    ]
                    { url = url
                    , label = E.text text
                    }
                ]
        )
        |> Mark.field "text" Mark.string
        |> Mark.field "url" Mark.string
        |> Mark.field "color" Mark.string
        |> Mark.field "background" Mark.string
        |> Mark.toBlock


treeBlock : WindowSize -> Mark.Block (E.Element msg)
treeBlock size =
    Mark.tree "Tree" (renderTree size) textBlock


renderTree : WindowSize -> Mark.Enumerated (List (E.Element msg)) -> E.Element msg
renderTree size (Mark.Enumerated list) =
    if List.length list.items > 0 then
        E.column [ paddingAuto size 0 ] <|
            List.map (renderItem size) list.items

    else
        E.none


renderItem : WindowSize -> Mark.Item (List (E.Element msg)) -> E.Element msg
renderItem size (Mark.Item item) =
    E.row []
        [ E.column [ E.paddingXY 10 6, E.height E.fill, E.alignTop ] [ E.el [] (E.text "‣") ]
        , E.column [ E.padding 5, E.spacing 12, E.width E.fill ] <|
            List.map (\i -> E.paragraph [ E.spacing 10 ] i) item.content
                ++ [ E.row [ E.spacing 12 ] [ renderTree size item.children ] ]
        ]


footerBlock : WindowSize -> Mark.Block (E.Element msg)
footerBlock size =
    Mark.record "Footer"
        (\background color height links ->
            E.el
                [ E.height <| E.px height
                , Background.color (decodeColor background)
                , Font.size 20
                , E.width E.fill
                , Region.footer
                , E.centerX
                ]
            <|
                E.wrappedRow
                    [ E.centerX
                    , E.centerY
                    , E.spacing 20
                    , E.width E.fill
                    , E.paddingXY (size.w // 5) 0
                    , Font.color (decodeColor color)
                    ]
                    links
        )
        |> Mark.field "background" Mark.string
        |> Mark.field "color" Mark.string
        |> Mark.field "height" Mark.int
        |> Mark.field "links" (Mark.tree "Tree" renderInlineTree textBlock)
        |> Mark.toBlock


renderInlineTree : Mark.Enumerated (List (E.Element msg)) -> List (E.Element msg)
renderInlineTree (Mark.Enumerated list) =
    List.map renderInlineItem list.items


renderInlineItem : Mark.Item (List (E.Element msg)) -> E.Element msg
renderInlineItem (Mark.Item item) =
    E.row [ E.width E.fill ] <|
        List.map (\i -> E.paragraph [ Font.center ] i) item.content


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
