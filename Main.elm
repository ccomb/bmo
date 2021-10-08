module Main exposing (main)

import Browser exposing (element)
import Element exposing (Element, centerX, paddingEach, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


type alias Name =
    String


type alias InitialValue =
    Float


type Variable
    = Variable Name InitialValue


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
    | InitialPointChanged Point


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
            ( { model | formula = f }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color (rgb255 42 44 43)
        , Font.color (rgb255 217 203 158)
        , paddingEach { top = 100, right = 0, bottom = 100, left = 0 }
        ]
    <|
        row [ centerX ]
            [ formulaInput model ]


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


formulaInput : Model -> Element Msg
formulaInput model =
    Input.text [ width (px 500), Font.color (rgb255 50 50 50) ]
        { onChange = FormulaChanged
        , text = Maybe.withDefault "" model.formula
        , placeholder = placeholder model
        , label = Input.labelAbove [ Font.size 30 ] (text "Enter your formula:")
        }


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
