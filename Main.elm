module Main exposing (main)

import Browser exposing (element)
import Char exposing (isAlpha)
import Dict
import Element exposing (Element, centerX, column, fill, maximum, padding, paddingEach, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as Decode
import Process
import Result
import Set
import Task exposing (perform)
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
    }


type Msg
    = FormulaChanged String
    | InitialValueChanged String String
    | GotVariables (Result Http.Error Point)
    | GotResult (Result Http.Error Point)



----------
-- INIT --
----------


defaultPlaceholder : String
defaultPlaceholder =
    "218*t*p*f - (p+s)*(b*1.38+12*n)+c = 0"


init : String -> ( Model, Cmd Msg )
init host =
    ( { formula = Nothing
      , initialPoint = Nothing
      , nearestPoint = Nothing
      , host = host
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
            ( { model | formula = f }
            , getVariables model.host inputstring
            )

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
                ( { model | initialPoint = newpoint }
                , getResult { model | initialPoint = newpoint, nearestPoint = Nothing }
                )
                -- TODO add delay

            else
                ( { model | initialPoint = newpoint, nearestPoint = Nothing }
                , Cmd.none
                )

        GotVariables result ->
            case result of
                Ok point ->
                    ( { model | initialPoint = Just point }, Cmd.none )

                Err error ->
                    ( { model | initialPoint = Nothing }, Cmd.none )

        GotResult result ->
            case result of
                Ok point ->
                    ( { model | nearestPoint = Just point }, Cmd.none )

                Err error ->
                    ( { model | nearestPoint = Nothing }, Cmd.none )


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
                            if v == "" then
                                False

                            else
                                True
                )
                p



---------------------
-- TALK TO BACKEND --
---------------------


urlBase : Host -> String
urlBase host =
    "http://" ++ host ++ ":8000"


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


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


queryParam : Variable -> Url.QueryParameter
queryParam variable =
    case variable of
        Variable name value ->
            Url.string name (value |> String.toFloat |> Maybe.map String.fromFloat |> Maybe.withDefault "invalid")


getVariables : Host -> String -> Cmd Msg
getVariables host inputstring =
    Http.get
        { url = urlBase host ++ queryFormula inputstring
        , expect = Http.expectJson GotVariables resultDecoder
        }


getResult : Model -> Cmd Msg
getResult model =
    case model.formula of
        Just f ->
            Http.get
                { url = urlBase model.host ++ queryResult model.formula model.initialPoint
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
        , paddingEach { top = 100, right = 0, bottom = 100, left = 0 }
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
        case model.initialPoint of
            Nothing ->
                [ Element.text "Please write a valid formula to be able to fill in the initial values" ]

            Just point ->
                List.map (\v -> row [] [ inputValue v ]) point


inputValue : Variable -> Element Msg
inputValue var =
    case var of
        Variable name value ->
            Input.text
                [ width (px 200)
                , Font.color (rgb255 50 50 50)
                ]
                { onChange = InitialValueChanged name
                , text = value
                , placeholder = Nothing
                , label = Input.labelLeft [ Font.size 30 ] (text <| name ++ " =")
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
                if isFilled (Just initPoint) then
                    case model.nearestPoint of
                        Just p ->
                            [ Element.text "The nearest solution is" ]
                                ++ List.map (\v -> row [] [ displayValue v ]) p

                        Nothing ->
                            [ Element.text "Could not compute the nearest point" ]

                else
                    [ Element.text "Fill in the initial values to compute the nearest solution" ]



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
