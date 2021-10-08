module Main exposing (main)

import Browser exposing (element)
import Char exposing (isAlpha)
import Element exposing (Element, centerX, column, padding, paddingEach, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Set


type alias Name =
    String


type Variable
    = Variable Name (Maybe Float)


type alias Point =
    List Variable


type alias Model =
    { formula : Maybe String
    , initial_point : Point
    , nearest_solution : Maybe Point
    }


defaultPlaceholder : String
defaultPlaceholder =
    "218*t*p*f - (p+s)*(b*1.38+12*n)+c = 0"


init : String -> ( Model, Cmd Msg )
init _ =
    ( { formula = Nothing
      , initial_point = []
      , nearest_solution = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


type Msg
    = FormulaChanged String
    | InitialValueChanged String String


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
            ( { model | formula = f, initial_point = extractVariables formula }, Cmd.none )

        InitialValueChanged name value ->
            let
                newpoint =
                    model.initial_point
                        |> List.map
                            (\variable ->
                                case variable of
                                    Variable n v ->
                                        if name == n then
                                            Variable name <| String.toFloat value

                                        else
                                            variable
                            )
            in
            ( { model | initial_point = newpoint }, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color (rgb255 42 44 43)
        , Font.color (rgb255 217 203 158)
        , paddingEach { top = 100, right = 0, bottom = 100, left = 0 }
        ]
    <|
        column [ centerX, spacing 20 ] <|
            [ inputFormula model ]
                ++ inputPoint model.initial_point


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


inputFormula : Model -> Element Msg
inputFormula model =
    row
        [ padding 50
        , Background.color (rgb255 70 70 70)
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
        |> List.map (\w -> Variable w Nothing)


inputValue : Variable -> Element Msg
inputValue var =
    case var of
        Variable name value ->
            Input.text
                [ width (px 200)
                , Font.color (rgb255 50 50 50)
                ]
                { onChange = InitialValueChanged name
                , text = value |> Maybe.map String.fromFloat |> Maybe.withDefault ""
                , placeholder = Nothing
                , label = Input.labelLeft [ Font.size 30 ] (text <| "Initial value for " ++ name ++ " =")
                }


inputPoint : Point -> List (Element Msg)
inputPoint point =
    point
        |> List.map (\v -> row [] [ inputValue v ])



--initialpointInput : Model -> Element Msg
--initialpointInput model =
--    model.initial_point
--        |>


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
