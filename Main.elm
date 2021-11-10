module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation as Nav
import Char exposing (isAlpha)
import Debouncer.Messages as Debouncer exposing (Debouncer, fromSeconds, provideInput, settleWhenQuietFor, toDebouncer)
import Dict
import Element exposing (Attribute, Element, centerX, column, el, fill, maximum, padding, paddingEach, paddingXY, paragraph, px, rgb255, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode
import Process
import Result
import Route exposing (Route, parseUrl)
import Set
import Task
import Tuple exposing (first, second)
import Url exposing (Url)
import Url.Builder as UrlBuilder
import Url.Parser exposing ((<?>), Parser, parse, query, string, top)
import Url.Parser.Query as Query



-----------
-- TYPES --
-----------


type Variable
    = Variable String String


type alias Point =
    List Variable


type alias Host =
    String


type alias Formula =
    String


type alias Model =
    { formula : Formula
    , initialPoint : Point
    , nearestPoint : Point
    , host : Host
    , debouncer : Debouncer Msg
    , spinnerV : Bool
    , spinnerR : Bool
    , navkey : Nav.Key
    , route : Route
    , error : Maybe String
    }


type Msg
    = FormulaChanged String
    | GetVariables
    | GotVariables (Result Http.Error OptimizationResult)
    | InitialValueChanged String String
    | GetResult
    | GotResult (Result Http.Error OptimizationResult)
    | Delay (Debouncer.Msg Msg)
    | LinkClicked UrlRequest
    | UrlChanged Url


type alias OptimizationResult =
    { point : Point
    , id : Maybe String
    }


type alias Flags =
    { host : Host
    , formula : Formula
    , initialPoint : Point

    --    , closest_solution : Point
    }



----------
-- INIT --
----------


defaultPlaceholder : String
defaultPlaceholder =
    ""


formulaParser : Parser (Maybe Formula -> a) a
formulaParser =
    query <| Query.string "formula"


initialVariableParser : String -> Parser (Maybe String -> a) a
initialVariableParser variable =
    query <| Query.string variable



-- TODO : deduce host from url??


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init value url key =
    let
        formula =
            parse formulaParser url |> Maybe.withDefault Nothing |> Maybe.withDefault ""

        flags =
            case Decode.decodeValue flagsDecoder value of
                Ok f ->
                    f

                Err error ->
                    { host = "plop", formula = "error=0", initialPoint = [] }

        model =
            { formula =
                if formula /= "" then
                    formula

                else
                    flags.formula
            , initialPoint = flags.initialPoint
            , nearestPoint = []
            , host = flags.host
            , debouncer = Debouncer.manual |> settleWhenQuietFor (Just <| fromSeconds 1.5) |> toDebouncer
            , spinnerV =
                if formula /= "" then
                    True

                else
                    False
            , spinnerR =
                if isFilled flags.initialPoint then
                    True

                else
                    False
            , route = parseUrl url
            , navkey = key
            , error = Nothing
            }
    in
    ( model
    , Task.perform ((\_ -> GetVariables) >> provideInput >> Delay)
        (Task.succeed "")
    )



------------
-- UPDATE --
------------


updateDebouncer : Debouncer.UpdateConfig Msg Model
updateDebouncer =
    { mapMsg = Delay
    , getDebouncer = .debouncer
    , setDebouncer = \d model -> { model | debouncer = d }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormulaChanged inputstring ->
            ( { model
                | formula = inputstring
                , spinnerV = True
                , initialPoint =
                    if inputstring == "" then
                        []

                    else
                        model.initialPoint
                , nearestPoint =
                    if inputstring == "" then
                        []

                    else
                        model.nearestPoint
              }
            , Task.perform ((\_ -> GetVariables) >> provideInput >> Delay)
                (Task.succeed "")
            )

        GetVariables ->
            ( { model | spinnerV = True }, Cmd.batch [ getVariables model, Nav.replaceUrl model.navkey <| buildFormulaUrl model ] )

        InitialValueChanged name value ->
            let
                newpoint =
                    List.map
                        (\(Variable n v) ->
                            if name == n then
                                Variable n value

                            else
                                Variable n v
                        )
                        model.initialPoint
            in
            if isFilled newpoint then
                ( { model | initialPoint = newpoint, spinnerR = True }
                , Task.perform ((\_ -> GetResult) >> provideInput >> Delay) (Task.succeed "")
                )

            else
                ( { model | initialPoint = newpoint, nearestPoint = [] }
                , Cmd.none
                )

        GotVariables result ->
            case result of
                Ok optimresult ->
                    let
                        newpoint =
                            updatePoint optimresult.point model.initialPoint
                    in
                    ( { model | initialPoint = newpoint, nearestPoint = [], spinnerV = False }
                    , if isFilled newpoint then
                        getResult model

                      else
                        Cmd.none
                    )

                Err error ->
                    ( { model
                        | error =
                            if model.formula /= "" then
                                httpErrorToString error

                            else
                                Nothing
                        , initialPoint = []
                        , nearestPoint = []
                        , spinnerV = False
                      }
                    , Cmd.none
                    )

        GetResult ->
            ( { model | spinnerR = True }, getResult model )

        GotResult optresult ->
            case optresult of
                Ok result ->
                    ( { model | nearestPoint = result.point, spinnerR = False }
                    , Nav.replaceUrl model.navkey <| buildIdUrl model result
                    )

                Err error ->
                    ( { model | nearestPoint = [], spinnerR = False, error = httpErrorToString error }
                    , Cmd.none
                    )

        Delay subMsg ->
            Debouncer.update update updateDebouncer subMsg model

        UrlChanged url ->
            ( { model | route = parseUrl url }
            , Cmd.none
            )

        LinkClicked urlrequest ->
            case urlrequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navkey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )


httpErrorToString : Http.Error -> Maybe String
httpErrorToString error =
    case error of
        Http.BadBody err ->
            err |> String.split "\n" |> List.reverse |> List.head

        _ ->
            Just "Other error"


isFilled : Point -> Bool
isFilled point =
    if List.length point == 0 then
        False

    else
        List.all
            (\(Variable n v) ->
                String.toFloat v |> Maybe.map (always True) |> Maybe.withDefault False
            )
            point


updatePoint :
    Point
    -> Point
    -> Point -- copy values from point to newpoint
updatePoint newpoint point =
    let
        existingvalues =
            point
                |> List.map
                    (\(Variable k v) ->
                        ( k, v )
                    )
                |> Dict.fromList
    in
    newpoint
        |> List.map
            (\(Variable k v) ->
                Variable k
                    (Dict.get k existingvalues |> Maybe.withDefault "")
            )


buildFormulaUrl : Model -> String
buildFormulaUrl model =
    UrlBuilder.absolute [] [ UrlBuilder.string "formula" model.formula ]


buildIdUrl : Model -> OptimizationResult -> String
buildIdUrl model result =
    Maybe.withDefault (buildFormulaUrl model) result.id



---------------------
-- TALK TO BACKEND --
---------------------


queryFormula : String -> String
queryFormula inputstring =
    UrlBuilder.absolute [ "optimize" ] [ UrlBuilder.string "formula" inputstring ]


queryResult : Formula -> Point -> String
queryResult inputstring point =
    UrlBuilder.absolute [ "optimize" ]
        ([ UrlBuilder.string "formula" inputstring ] ++ List.map queryParam point)


queryParam : Variable -> UrlBuilder.QueryParameter
queryParam (Variable name value) =
    UrlBuilder.string name (value |> String.toFloat |> Maybe.map String.fromFloat |> Maybe.withDefault "")


getVariables : Model -> Cmd Msg
getVariables model =
    Http.get
        { url = model.host ++ queryFormula model.formula
        , expect = Http.expectJson GotVariables resultDecoder
        }


getResult : Model -> Cmd Msg
getResult model =
    Http.get
        { url = model.host ++ queryResult model.formula model.initialPoint
        , expect = Http.expectJson GotResult resultDecoder
        }


pointDecoder : Decode.Decoder Point
pointDecoder =
    Decode.dict (Decode.maybe Decode.float)
        |> Decode.andThen
            (\dict ->
                Decode.succeed
                    (Dict.toList dict
                        |> List.map
                            (\( k, v ) -> Variable k (Maybe.withDefault "" <| Maybe.map String.fromFloat v))
                    )
            )


resultDecoder : Decode.Decoder OptimizationResult
resultDecoder =
    Decode.field "status" Decode.string
        |> Decode.andThen
            (\s ->
                if s == "success" then
                    Decode.map2 OptimizationResult
                        (Decode.field "point" pointDecoder)
                        (Decode.field "id" (Decode.maybe Decode.string))

                else
                    Decode.fail s
            )


flagsDecoder : Decode.Decoder Flags
flagsDecoder =
    Decode.map3 Flags
        (Decode.field "host" Decode.string)
        (Decode.field "formula" Decode.string)
        (Decode.field "initial_point" pointDecoder)



--        (Decode.field "closest_solution" pointDecoder)
-----------
-- VIEWS --
-----------


view : Model -> Document Msg
view model =
    { title = "BMO"
    , body =
        [ Element.layout
            [ Background.color (rgb255 42 44 43)
            , Font.color (rgb255 217 203 158)
            , paddingEach { top = 10, right = 0, bottom = 100, left = 0 }
            ]
          <|
            column [ centerX, spacing 20, width <| maximum 700 fill ] <|
                [ inputFormula model
                , initialPoint model
                , nearestPoint model
                ]
        ]
    }


inputFormula : Model -> Element Msg
inputFormula model =
    row (blockAttributes ++ [ paddingEach { blockEdges | top = 50 } ])
        [ Input.text
            [ width fill
            , Font.color (rgb255 50 50 50)
            , Input.focusedOnLoad
            ]
            { onChange = FormulaChanged
            , text = model.formula
            , placeholder = placeholder model
            , label = Input.labelAbove [ Font.size 30 ] (text "Enter your formula:")
            }
        ]


placeholder : Model -> Maybe (Input.Placeholder msg)
placeholder model =
    Just <|
        Input.placeholder []
            (text <|
                if model.formula == "" then
                    defaultPlaceholder

                else
                    model.formula
            )


spinnerImage : Element Msg
spinnerImage =
    Element.image [] { src = "/static/spinner.png", description = "spinner" }


blockEdges =
    { top = 20, bottom = 50, left = 50, right = 50 }


blockAttributes : List (Attribute Msg)
blockAttributes =
    [ Background.color (rgb255 70 70 70)
    , spacing 20
    , paddingEach blockEdges
    , centerX
    , width fill
    , Border.rounded 15
    ]


initialPoint : Model -> Element Msg
initialPoint model =
    if model.spinnerV then
        column blockAttributes [ spinnerImage ]

    else if model.initialPoint /= [] then
        column blockAttributes <|
            [ row [] [ text "Your current situation:" ] ]
                ++ List.map (\v -> wrappedRow [ spacing 10 ] [ inputLabel v, inputValue v ]) model.initialPoint

    else
        Element.none


inputLabel : Variable -> Element Msg
inputLabel (Variable name value) =
    el [] (text name)


inputValue : Variable -> Element Msg
inputValue (Variable name value) =
    Input.text
        [ width fill
        , Font.color (rgb255 50 50 50)
        ]
        { onChange = InitialValueChanged name
        , text = value
        , placeholder = Nothing
        , label = Input.labelHidden name
        }


displayValue : Variable -> Element Msg
displayValue (Variable name value) =
    text <|
        (value
            |> String.toFloat
            |> Maybe.map String.fromFloat
            |> Maybe.withDefault "N/A"
        )


variation : Variable -> Variable -> Element Msg
variation (Variable initial_name initial_value) (Variable target_name target_value) =
    case String.toFloat initial_value of
        Just initial ->
            case String.toFloat target_value of
                Just target ->
                    let
                        v =
                            (target - initial) / initial * 100 |> truncate

                        direction =
                            if v > 0 then
                                "Raise "

                            else
                                "Lower "

                        colorize =
                            \x ->
                                if x >= 0 then
                                    Font.color (rgb255 0 200 0)

                                else
                                    Font.color (rgb255 200 0 0)
                    in
                    paragraph
                        []
                        [ el
                            [ colorize v ]
                            (text direction)
                        , el [ Font.bold ] (text target_name)
                        , text " by "
                        , el [ Font.bold, colorize v ] (text <| String.fromInt v ++ " %")
                        ]

                Nothing ->
                    Element.none

        Nothing ->
            Element.none


approx : Float -> Float -> Float
approx precision x =
    -- precision = nb chiffres après la virgule
    x |> (*) (10 ^ precision) |> truncate |> toFloat |> (*) (10 ^ -precision)


neverFloat : Never -> Float
neverFloat =
    never


nearestPoint : Model -> Element Msg
nearestPoint model =
    if model.spinnerR then
        column blockAttributes [ spinnerImage ]

    else if model.initialPoint /= [] then
        if not <| isFilled model.initialPoint then
            column blockAttributes
                [ paragraph [] [ text "Fill in the initial values to compute the nearest solution" ] ]

        else
            column blockAttributes <|
                [ text "To achieve your goals you should:" ]
                    ++ List.map2
                        (\iv tv ->
                            row []
                                [ variation iv tv
                                , text " to reach a value of "
                                , displayValue tv
                                ]
                        )
                        model.initialPoint
                        model.nearestPoint

    else
        Maybe.map (\err -> column blockAttributes [ viewError model ]) model.error |> Maybe.withDefault Element.none


notfound : Model -> Element Msg
notfound model =
    el [] (text "Not Found")


viewError : Model -> Element Msg
viewError model =
    Maybe.map
        (\t ->
            Element.paragraph
                [ Font.color (rgb255 255 0 0)
                , Background.color (rgb255 255 255 255)
                , padding 10
                ]
                [ text t ]
        )
        model.error
        |> Maybe.withDefault Element.none



------------------------
-- OTHER SIDE EFFECTS --
------------------------


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


onurlrequest : UrlRequest -> Msg
onurlrequest =
    LinkClicked


onurlchange : Url -> Msg
onurlchange =
    UrlChanged



----------
-- MAIN --
----------


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onurlrequest
        , onUrlChange = onurlchange
        }
