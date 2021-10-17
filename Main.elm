module Main exposing (main)

import Browser exposing (element)
import Char exposing (isAlpha)
import Debouncer.Messages as Debouncer exposing (Debouncer, fromSeconds, provideInput, settleWhenQuietFor, toDebouncer)
import Dict
import Element exposing (Element, centerX, column, el, fill, maximum, padding, paddingEach, paragraph, px, rgb255, row, spacing, text, width, wrappedRow)
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
import Set
import Task
import Tuple exposing (first, second)
import Url.Builder as Url



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
    Maybe String


type alias Model =
    { formula : Formula
    , initialPoint : Maybe Point
    , nearestPoint : Maybe Point
    , host : Host
    , debouncer : Debouncer Msg
    , spinnerV : Bool
    , spinnerR : Bool
    }


type Msg
    = FormulaChanged String
    | GetVariables
    | GotVariables (Result Http.Error Point)
    | InitialValueChanged String String
    | GetResult
    | GotResult (Result Http.Error Point)
    | Delay (Debouncer.Msg Msg)



----------
-- INIT --
----------


defaultPlaceholder : String
defaultPlaceholder =
    ""


init : String -> ( Model, Cmd Msg )
init host =
    ( { formula = Nothing
      , initialPoint = Nothing
      , nearestPoint = Nothing
      , host = host
      , debouncer = Debouncer.manual |> settleWhenQuietFor (Just <| fromSeconds 1.5) |> toDebouncer
      , spinnerV = False
      , spinnerR = False
      }
    , Cmd.none
    )



------------------
-- SUBSCRIPTION --
------------------


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none



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
            let
                f =
                    if String.length inputstring == 0 then
                        Nothing

                    else
                        Just inputstring
            in
            if f == Nothing then
                ( { model | formula = Nothing, initialPoint = Nothing, nearestPoint = Nothing, spinnerV = False }, Cmd.none )

            else
                ( { model | formula = f, spinnerV = True }
                , Task.perform ((\_ -> GetVariables) >> provideInput >> Delay)
                    (Task.succeed "")
                )

        GetVariables ->
            ( { model | spinnerV = True }, getVariables model )

        InitialValueChanged name value ->
            let
                newpoint =
                    Maybe.map
                        (List.map
                            (\variable ->
                                case variable of
                                    Variable n v ->
                                        if name == n then
                                            Variable name value

                                        else
                                            variable
                            )
                        )
                        model.initialPoint
            in
            if isFilled newpoint then
                ( { model | initialPoint = newpoint, spinnerR = True }
                , Task.perform ((\_ -> GetResult) >> provideInput >> Delay) (Task.succeed "")
                )

            else
                ( { model | initialPoint = newpoint, nearestPoint = Nothing }
                , Cmd.none
                )

        GotVariables result ->
            case result of
                Ok point ->
                    let
                        newpoint =
                            updatePoint point model.initialPoint
                    in
                    ( { model | initialPoint = newpoint, nearestPoint = Nothing, spinnerV = False }
                    , if isFilled newpoint then
                        getResult model

                      else
                        Cmd.none
                    )

                Err error ->
                    ( { model | nearestPoint = Nothing, spinnerV = False }
                    , Cmd.none
                    )

        GetResult ->
            ( { model | spinnerR = True }, getResult model )

        GotResult result ->
            case result of
                Ok point ->
                    ( { model | nearestPoint = Just point, spinnerR = False }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | nearestPoint = Nothing, spinnerR = False }
                    , Cmd.none
                    )

        Delay subMsg ->
            Debouncer.update update updateDebouncer subMsg model


isFilled : Maybe Point -> Bool
isFilled point =
    case point of
        Nothing ->
            False

        Just p ->
            List.all
                (\var ->
                    case var of
                        Variable n v ->
                            String.toFloat v |> Maybe.map (always True) |> Maybe.withDefault False
                )
                p


updatePoint : Point -> Maybe Point -> Maybe Point
updatePoint newpoint point =
    let
        existingvalues =
            point
                |> Maybe.map
                    (List.map
                        (\var ->
                            case var of
                                Variable k v ->
                                    ( k, v )
                        )
                    )
                |> Maybe.withDefault []
                |> Dict.fromList
    in
    newpoint
        |> List.map
            (\var ->
                case var of
                    Variable k v ->
                        Variable k (Dict.get k existingvalues |> Maybe.withDefault "")
            )
        |> Just



---------------------
-- TALK TO BACKEND --
---------------------


queryFormula : String -> String
queryFormula inputstring =
    Url.absolute [ "optimize" ] [ Url.string "formula" inputstring ]


queryResult : Formula -> Maybe Point -> String
queryResult inputstring point =
    case point of
        Nothing ->
            Url.absolute [ "optimize" ] [ Url.string "formula" (Maybe.withDefault "" inputstring) ]

        Just p ->
            Url.absolute [ "optimize" ]
                ([ Url.string "formula" (Maybe.withDefault "" inputstring) ] ++ List.map queryParam p)


queryParam : Variable -> Url.QueryParameter
queryParam variable =
    case variable of
        Variable name value ->
            Url.string name (value |> String.toFloat |> Maybe.map String.fromFloat |> Maybe.withDefault "invalid")


getVariables : Model -> Cmd Msg
getVariables model =
    case model.formula of
        Nothing ->
            Cmd.none

        Just f ->
            Http.get
                { url = model.host ++ queryFormula f
                , expect = Http.expectJson GotVariables resultDecoder
                }


getResult : Model -> Cmd Msg
getResult model =
    case model.formula of
        Just f ->
            Http.get
                { url = model.host ++ queryResult model.formula model.initialPoint
                , expect = Http.expectJson GotResult resultDecoder
                }

        Nothing ->
            Cmd.none


pointDecoder : Decode.Decoder Point
pointDecoder =
    Decode.dict (Decode.maybe Decode.float) |> Decode.andThen (\dict -> Decode.succeed (Dict.toList dict |> List.map (\var -> Variable (first var) <| Maybe.withDefault "" <| Maybe.map String.fromFloat (second var))))


resultDecoder : Decode.Decoder Point
resultDecoder =
    Decode.field "status" Decode.string
        |> Decode.andThen
            (\s ->
                if s == "success" then
                    Decode.field "result" pointDecoder

                else
                    Decode.field "error"
                        (Decode.fail "error")
            )



-----------
-- VIEWS --
-----------


view : Model -> Html Msg
view model =
    Element.layout
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


inputFormula : Model -> Element Msg
inputFormula model =
    row
        [ padding 50
        , Background.color (rgb255 70 70 70)
        , Border.rounded 15
        , width fill
        ]
        [ Input.text
            [ width fill
            , Font.color (rgb255 50 50 50)
            , Input.focusedOnLoad
            ]
            { onChange = FormulaChanged
            , text = Maybe.withDefault "" model.formula
            , placeholder = placeholder model
            , label = Input.labelAbove [ Font.size 30 ] (text "Enter your formula:")
            }
        ]


placeholder : Model -> Maybe (Input.Placeholder msg)
placeholder model =
    Just <|
        Input.placeholder []
            (text <|
                case model.formula of
                    Nothing ->
                        defaultPlaceholder

                    Just f ->
                        ""
            )


spinnerImage : Element Msg
spinnerImage =
    Element.image [] { src = "/static/spinner.png", description = "spinner" }


initialPoint : Model -> Element Msg
initialPoint model =
    column
        [ Background.color (rgb255 70 70 70)
        , spacing 20
        , padding 50
        , centerX
        , width fill
        , Border.rounded 15
        ]
    <|
        if model.spinnerV then
            [ spinnerImage ]

        else
            case model.initialPoint of
                Nothing ->
                    [ paragraph [] [ text "Please write a valid formula to be able to fill in the initial values" ] ]

                Just point ->
                    List.map (\v -> wrappedRow [ spacing 10 ] [ inputLabel v, inputValue v ]) point


inputLabel : Variable -> Element Msg
inputLabel var =
    case var of
        Variable name value ->
            el [] (text name)


inputValue : Variable -> Element Msg
inputValue var =
    case var of
        Variable name value ->
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
displayValue var =
    case var of
        Variable name value ->
            Element.text (name ++ " = " ++ value)


nearestPoint : Model -> Element Msg
nearestPoint model =
    case model.initialPoint of
        Nothing ->
            Element.none

        Just initPoint ->
            column
                [ Background.color (rgb255 70 70 70)
                , spacing 20
                , padding 50
                , centerX
                , width fill
                , Border.rounded 15
                ]
            <|
                if model.spinnerR then
                    [ spinnerImage ]

                else if isFilled (Just initPoint) then
                    case model.nearestPoint of
                        Just p ->
                            [ Element.text "The nearest solution is" ]
                                ++ List.map (\v -> row [] [ displayValue v ]) p

                        Nothing ->
                            [ Element.none ]

                else
                    [ paragraph [] [ text "Fill in the initial values to compute the nearest solution" ] ]



----------
-- MAIN --
----------


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
