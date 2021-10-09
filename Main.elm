module Main exposing (fromFloat2, main)

import Browser exposing (element)
import Char exposing (isAlpha)
import Dict
import Element exposing (Element, centerX, column, padding, paddingEach, px, rgb255, row, spacing, text, width)
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


type alias Model =
    { formula : Maybe String
    , initialPoint : Point
    , nearestPoint : Maybe Point
    , host : String
    }


type Msg
    = FormulaChanged String
    | InitialValueChanged String String
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
      , initialPoint = []
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


extractVariables : String -> List Variable
extractVariables formula =
    formula
        |> String.map
            (\c ->
                if isAlpha c then
                    c

                else
                    ' '
            )
        |> String.words
        |> Set.fromList
        |> Set.remove ""
        |> Set.toList
        |> List.map (\w -> Variable w "")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormulaChanged formula ->
            let
                f =
                    if String.length formula == 0 then
                        Nothing

                    else
                        Just formula
            in
            ( { model | formula = f, initialPoint = extractVariables formula }, Cmd.none )

        InitialValueChanged name value ->
            let
                newpoint =
                    model.initialPoint
                        |> List.map
                            (\variable ->
                                case variable of
                                    Variable n v ->
                                        if name == n then
                                            Variable name value

                                        else
                                            variable
                            )
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

        GotResult result ->
            case result of
                Ok point ->
                    ( { model | nearestPoint = Just point }, Cmd.none )

                Err error ->
                    ( { model | nearestPoint = Nothing }, Cmd.none )


isFilled : Point -> Bool
isFilled point =
    List.all
        (\var ->
            case var of
                Variable n v ->
                    if v == "" then
                        False

                    else
                        True
        )
        point



------------------
-- BACKEND COMM --
------------------


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


queryParam : Variable -> Url.QueryParameter
queryParam variable =
    case variable of
        Variable name value ->
            Url.string name (value |> String.toFloat |> Maybe.map String.fromFloat |> Maybe.withDefault "invalid")


getResult : Model -> Cmd Msg
getResult model =
    case model.formula of
        Just f ->
            Http.get
                { url =
                    "http://"
                        ++ model.host
                        ++ ":8000"
                        ++ Url.absolute [ "optimize" ]
                            ([ Url.string "formula" f
                             ]
                                ++ List.map queryParam
                                    model.initialPoint
                            )
                , expect = Http.expectJson GotResult resultDecoder
                }

        Nothing ->
            Cmd.none


pointDecoder : Decode.Decoder Point
pointDecoder =
    Decode.dict Decode.float |> Decode.andThen (\d -> Decode.succeed (Dict.toList d |> List.map (\var -> Variable (first var) (String.fromFloat <| second var))))


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
        column [ centerX, spacing 20 ] <|
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
        ]
        [ Input.text
            [ width (px 500)
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


hasEqualSign : Maybe String -> Bool
hasEqualSign formula =
    case formula of
        Nothing ->
            False

        Just f ->
            if String.contains "=" f then
                True

            else
                False


initialPoint : Model -> Element Msg
initialPoint model =
    if hasEqualSign model.formula then
        column
            [ Background.color (rgb255 70 70 70)
            , spacing 20
            , padding 50
            , centerX
            , Border.rounded 15
            ]
        <|
            List.map (\v -> row [] [ inputValue v ]) model.initialPoint

    else
        Element.none


fromFloat2 : Float -> String
fromFloat2 =
    String.fromFloat
        >> String.split "."
        >> (\l ->
                if List.length l == 1 then
                    l ++ [ "" ]

                else
                    l
           )
        >> List.map
            (\n ->
                if n == "" then
                    "0"

                else
                    n
            )
        >> String.join "."


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
    if hasEqualSign model.formula then
        column
            [ Background.color (rgb255 70 70 70)
            , spacing 20
            , padding 50
            , centerX
            , Border.rounded 15
            ]
        <|
            case model.nearestPoint of
                Just p ->
                    [ Element.text "The nearest solution is" ]
                        ++ List.map (\v -> row [] [ displayValue v ]) p

                Nothing ->
                    if model.formula == Nothing then
                        []

                    else
                        [ Element.text "Fill in the initial values to compute the nearest solution" ]

    else
        Element.none



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
