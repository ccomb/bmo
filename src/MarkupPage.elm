module MarkupPage exposing (Flags, Model, Msg, init, subscriptions, update, view)

import Effect exposing (Effect)
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
    | ClosedIFrame String


type alias Flags =
    String


init : Flags -> ( Model, Effect Shared.Msg Msg )
init flags =
    ( { markup = Loading
      , openIFrames = Set.empty
      }
    , Effect.fromCmd <|
        Http.get
            { url =
                let
                    path =
                        if flags == "" then
                            "home_"

                        else
                            flags
                in
                "/content/" ++ path
            , expect = Http.expectString GotMarkup
            }
    )


update : Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update msg model =
    case msg of
        GotMarkup result ->
            case result of
                Ok markup ->
                    ( { model
                        | markup = Loaded markup
                      }
                    , Effect.none
                    )

                Err _ ->
                    -- TODO
                    ( { model
                        | markup = Failed "Could not load the content"
                      }
                    , Effect.none
                    )

        OpenedIFrame name ->
            ( { model | openIFrames = Set.insert name model.openIFrames }, Effect.none )

        ClosedIFrame name ->
            ( { model | openIFrames = Set.remove name model.openIFrames }, Effect.none )


view : Shared.Model -> Model -> View Msg
view shared model =
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
    E.paddingXY (size.w // 3 - 120)


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
    Mark.document
        (E.column
            [ E.width (E.px shared.windowSize.w)
            , Font.family
                [ Font.typeface "Gotu"
                ]
            , E.spacing 20
            ]
        )
        (Mark.manyOf
            [ hero shared
            , title shared 1
            , title shared 2
            , title shared 3
            , textSection shared
            , ctaBlock shared
            , imageBlock shared
            , iframeBlock model shared
            , treeBlock shared
            , footerBlock shared
            , sectionBlock shared
            ]
        )


hero : Shared.Model -> Mark.Block (E.Element msg)
hero shared =
    Mark.record "HERO"
        (\backgroundColor textColor img tagline titleBackground url text background color heightRatio ->
            E.row
                [ E.height <| E.px <| floor <| toFloat shared.windowSize.h * heightRatio
                , Background.color (decodeColor backgroundColor)
                , Background.image img
                , Region.heading 1
                , Font.size (shared.windowSize.h // 30 |> min 40 |> max 25)
                , Font.extraBold
                , E.width E.fill
                ]
                [ E.column
                    [ E.centerX
                    , E.width (E.px (max 375 (shared.windowSize.w // 2)))
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
        |> Mark.field "tagline" Mark.string
        |> Mark.field "titleBackground" Mark.string
        |> Mark.field "url" Mark.string
        |> Mark.field "text" Mark.string
        |> Mark.field "background" Mark.string
        |> Mark.field "color" Mark.string
        |> Mark.field "heightRatio" Mark.float
        |> Mark.toBlock


imageBlock : Shared.Model -> Mark.Block (E.Element msg)
imageBlock shared =
    Mark.record "Image"
        (\src desc ->
            E.column [ E.centerX, paddingAuto shared.windowSize 0 ]
                [ E.image [] { src = src, description = desc }
                ]
        )
        |> Mark.field "src" Mark.string
        |> Mark.field "desc" Mark.string
        |> Mark.toBlock


iframeBlock : Model -> Shared.Model -> Mark.Block (E.Element Msg)
iframeBlock model shared =
    Mark.record "iframe"
        (\name button color bcolor src width height background ->
            E.el
                [ E.width E.fill
                , paddingAuto shared.windowSize 0
                , Background.color (decodeColor background)
                ]
            <|
                if Set.member name model.openIFrames then
                    E.column []
                        [ E.row [ E.alignRight ]
                            [ Input.button
                                [ Background.color <| decodeColor bcolor
                                , E.spacing 30
                                , Font.color <| decodeColor color
                                , Font.bold
                                , E.paddingXY 20 10
                                , Border.rounded 10
                                , Border.shadow
                                    { color = E.rgb255 20 20 20
                                    , blur = 10
                                    , offset = ( 3, 3 )
                                    , size = 0.1
                                    }
                                ]
                                { onPress = Just (ClosedIFrame name), label = E.text "×" }
                            ]
                        , E.html <|
                            Html.iframe
                                [ Html.Attributes.width width
                                , Html.Attributes.height height
                                , Html.Attributes.src src
                                , Html.Attributes.style "border" "none"
                                ]
                                []
                        ]

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


link : Mark.Record (E.Element msg)
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


cta : Mark.Record (E.Element msg)
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


image : Mark.Record (E.Element msg)
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


textBlock : Shared.Model -> Mark.Block (E.Element msg)
textBlock shared =
    Mark.map (E.paragraph [ E.spacing 10 ]) <|
        Mark.textWith
            { view = \styles str -> E.el (fromMark styles) (E.text str)
            , replacements = []
            , inlines = [ link, cta, image, nb shared ]
            }


title : Shared.Model -> Int -> Mark.Block (E.Element msg)
title shared level =
    -- H1, H2, H3 title
    Mark.block ("H" ++ String.fromInt level)
        (\child ->
            E.el
                [ Region.heading level
                , Font.size (45 - 6 * level)
                , Font.heavy
                , paddingAuto shared.windowSize 20
                ]
                child
        )
        (textBlock shared)


ctaBlock : Shared.Model -> Mark.Block (E.Element msg)
ctaBlock shared =
    Mark.record "ctaBlock"
        (\text url color background ->
            E.row [ paddingAuto shared.windowSize 0 ]
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


treeBlock : Shared.Model -> Mark.Block (E.Element msg)
treeBlock shared =
    Mark.tree "TREE" (renderTree shared.windowSize) (Mark.map (\b -> [ b ]) (textBlock shared))


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


footerBlock : Shared.Model -> Mark.Block (E.Element msg)
footerBlock shared =
    Mark.record "Footer"
        (\background color links ->
            E.el
                [ Background.color (decodeColor background)
                , Font.size 20
                , E.paddingXY 20 20
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
                    , E.paddingXY (shared.windowSize.w // 5) 0
                    , Font.color (decodeColor color)
                    ]
                    links
        )
        |> Mark.field "background" Mark.string
        |> Mark.field "color" Mark.string
        |> Mark.field "links" (Mark.tree "TREE" renderTreeInline (Mark.map (\b -> [ b ]) (textBlock shared)))
        |> Mark.toBlock


renderTreeInline : Mark.Enumerated (List (E.Element msg)) -> List (E.Element msg)
renderTreeInline (Mark.Enumerated list) =
    List.map renderInlineItem list.items


renderInlineItem : Mark.Item (List (E.Element msg)) -> E.Element msg
renderInlineItem (Mark.Item item) =
    E.row [ E.width E.fill ] <|
        List.map (\i -> E.paragraph [ Font.center ] i) item.content


textSection : Shared.Model -> Mark.Block (E.Element msg)
textSection shared =
    Mark.map (E.el [ paddingAuto shared.windowSize 0 ]) (textBlock shared)


sectionBlock : Shared.Model -> Mark.Block (E.Element msg)
sectionBlock shared =
    Mark.record "SECTION"
        (\padding backgroundColor content color fontsize ->
            E.wrappedRow
                [ Background.color (decodeColor backgroundColor)
                , paddingAuto shared.windowSize padding
                , Font.size fontsize
                , E.width E.fill
                , Font.color (decodeColor color)
                ]
                [ E.paragraph [] [ content ] ]
        )
        |> Mark.field "padding" Mark.int
        |> Mark.field "backgroundColor" Mark.string
        |> Mark.field "content" (textBlock shared)
        |> Mark.field "color" Mark.string
        |> Mark.field "fontsize" Mark.int
        |> Mark.toBlock


nb : Shared.Model -> Mark.Record (E.Element msg)
nb shared =
    Mark.annotation "nb"
        -- TODO how to specify nbsimu from the markup?
        (\_ -> E.text <| String.fromInt shared.nbsimu)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
