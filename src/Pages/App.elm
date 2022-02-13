module Pages.App exposing (Model, Msg, page)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation as Nav
import Debouncer.Messages as Debouncer exposing (Debouncer)
import Dict
import Dropdown
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Gen.Params.App exposing (Params)
import Gen.Route exposing (Route(..))
import Html exposing (Html)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode
import Optim exposing (..)
import Page
import Process
import Request exposing (Request)
import Result
import Round
import Route exposing (Route)
import Set
import Shared
import Task
import Tuple exposing (first, second)
import Url exposing (Url)
import Url.Builder as UrlBuilder
import Url.Parser as Parser exposing (Parser)
import Url.Parser.Query as Query
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init shared req
        , update = update req
        , view = view
        , subscriptions = subscriptions
        }



-----------
-- TYPES --
-----------


type alias Objective =
    String


type alias Model =
    { formula : Formula
    , initialPoint : Point
    , coefs : Coefs
    , nearestPoint : Point
    , debouncer : Debouncer Msg
    , spinnerV : Bool
    , spinnerR : Bool
    , error : Maybe String
    , dropdownState : Dropdown.State Objective
    , selectedObjective : Maybe Objective
    }


type Msg
    = FormulaChanged String
    | GetVariables
    | GotVariables (Result Http.Error OptimizationResult)
    | InitialValueChanged String String
    | CoefChanged String Float
    | GetResult
    | GotResult (Result Http.Error OptimizationResult)
    | Delay (Debouncer.Msg Msg)
    | OptionPicked (Maybe Objective)
    | DropdownMsg (Dropdown.Msg Objective)



----------
-- INIT --
----------


defaultPlaceholder : String
defaultPlaceholder =
    ""


formulaParser : Parser (Maybe Formula -> a) a
formulaParser =
    Parser.query <| Query.string "formula"


initialVariableParser : String -> Parser (Maybe String -> a) a
initialVariableParser variable =
    Parser.query <| Query.string variable


init : Shared.Model -> Request.With Params -> ( Model, Cmd Msg )
init shared req =
    let
        formula =
            Dict.get "formula" req.query |> Maybe.withDefault ""

        initval =
            shared.initval

        model =
            { formula =
                if formula /= "" then
                    formula

                else
                    initval.formula
            , initialPoint = initval.initialPoint
            , coefs = initval.coefs
            , nearestPoint = []
            , debouncer = Debouncer.manual |> Debouncer.settleWhenQuietFor (Just <| Debouncer.fromSeconds 1.5) |> Debouncer.toDebouncer
            , spinnerV =
                if formula /= "" then
                    True

                else
                    False
            , spinnerR =
                if isFilled initval.initialPoint then
                    True

                else
                    False
            , error = Nothing
            , dropdownState = Dropdown.init "Select your objective"
            , selectedObjective = Nothing
            }
    in
    ( model
    , Task.perform ((\_ -> GetVariables) >> Debouncer.provideInput >> Delay)
        (Task.succeed "")
    )


pointToNames : Point -> List String
pointToNames point =
    List.map (\(Variable n v) -> n) point


dropdownConfig : Model -> Dropdown.Config Objective Msg Model
dropdownConfig model =
    let
        containerAttrs =
            [ E.width (E.px 300) ]

        selectAttrs =
            [ Border.width 1, Border.rounded 5, E.paddingXY 16 8, E.spacing 10, E.width E.fill ]

        searchAttrs =
            [ Border.width 0, E.padding 0 ]

        listAttrs =
            [ Border.width 1
            , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
            , E.width E.fill
            , E.spacing 5
            ]

        itemToPrompt item =
            E.text item

        itemToElement selected highlighted i =
            let
                bgColor =
                    if highlighted then
                        E.rgb255 70 70 70

                    else if selected then
                        E.rgb255 80 80 80

                    else
                        E.rgb255 50 50 50
            in
            E.row
                [ Background.color bgColor
                , E.padding 8
                , E.spacing 10
                , E.width E.fill
                ]
                [ E.el [] (E.text "-")
                , E.el [ Font.size 16 ] (E.text i)
                ]
    in
    Dropdown.filterable
        { itemsFromModel = always <| pointToNames model.initialPoint
        , selectionFromModel = .selectedObjective
        , dropdownMsg = DropdownMsg
        , onSelectMsg = OptionPicked
        , itemToPrompt = itemToPrompt
        , itemToElement = itemToElement
        , itemToText = identity
        }
        |> Dropdown.withContainerAttributes containerAttrs
        |> Dropdown.withPromptElement (E.el [] (E.text "Select option"))
        |> Dropdown.withFilterPlaceholder "Type for option"
        |> Dropdown.withSelectAttributes selectAttrs
        |> Dropdown.withListAttributes listAttrs
        |> Dropdown.withSearchAttributes searchAttrs



------------
-- UPDATE --
------------


updateDebouncer : Debouncer.UpdateConfig Msg Model
updateDebouncer =
    { mapMsg = Delay
    , getDebouncer = .debouncer
    , setDebouncer = \d model -> { model | debouncer = d }
    }


update : Request.With Params -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
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
            , Task.perform ((\_ -> GetVariables) >> Debouncer.provideInput >> Delay)
                (Task.succeed "")
            )

        GetVariables ->
            ( { model | spinnerV = True }
            , Cmd.batch
                [ getVariables req model
                , Nav.replaceUrl req.key <| buildFormulaUrl model
                ]
            )

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

                newcoefs =
                    if
                        List.foldl
                            (\(Coef n v) b ->
                                if name == n then
                                    True

                                else
                                    b
                            )
                            False
                            model.coefs
                    then
                        model.coefs

                    else
                        Coef name 1.0 :: model.coefs
            in
            if isFilled newpoint then
                ( { model | initialPoint = newpoint, coefs = newcoefs, spinnerR = True }
                , Task.perform ((\_ -> GetResult) >> Debouncer.provideInput >> Delay) (Task.succeed "")
                )

            else
                ( { model | initialPoint = newpoint, coefs = newcoefs, nearestPoint = [] }
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
                        getResult req model

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
            ( { model | spinnerR = True }, getResult req model )

        GotResult optresult ->
            case optresult of
                Ok result ->
                    ( { model | nearestPoint = result.point, spinnerR = False }
                    , Nav.replaceUrl req.key <| buildIdUrl model result
                    )

                Err error ->
                    ( { model
                        | nearestPoint = []
                        , spinnerR = False
                        , error =
                            if model.nearestPoint /= [] then
                                httpErrorToString error

                            else
                                Nothing
                      }
                    , Cmd.none
                    )

        Delay subMsg ->
            Debouncer.update (update req) updateDebouncer subMsg model

        CoefChanged name coef ->
            let
                newcoefs =
                    List.map
                        (\(Coef n v) ->
                            if name == n then
                                Coef n coef

                            else
                                Coef n v
                        )
                        model.coefs
            in
            if isFilled model.initialPoint then
                ( { model | coefs = newcoefs, spinnerR = True }
                , Task.perform ((\_ -> GetResult) >> Debouncer.provideInput >> Delay) (Task.succeed "")
                )

            else
                ( { model | coefs = newcoefs, nearestPoint = [] }
                , Cmd.none
                )

        OptionPicked option ->
            ( { model | selectedObjective = option }
            , Task.perform ((\_ -> GetResult) >> Debouncer.provideInput >> Delay) (Task.succeed "")
            )

        DropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update (dropdownConfig model) subMsg model model.dropdownState
            in
            ( { model | dropdownState = state }, cmd )


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
    UrlBuilder.absolute [ "app" ] [ UrlBuilder.string "formula" model.formula ]


buildIdUrl : Model -> OptimizationResult -> String
buildIdUrl model result =
    result.id |> Maybe.map (\i -> UrlBuilder.absolute [ "app" ] [ UrlBuilder.string "id" i ]) |> Maybe.withDefault (buildFormulaUrl model)



---------------------
-- TALK TO BACKEND --
---------------------


queryFormula : String -> String
queryFormula inputstring =
    UrlBuilder.absolute [ "optimize" ] [ UrlBuilder.string "formula" inputstring ]


queryResult : Formula -> Point -> Coefs -> Maybe Objective -> String
queryResult inputstring point coefs objective =
    UrlBuilder.absolute [ "optimize" ]
        ([ UrlBuilder.string "formula" inputstring ]
            ++ List.map queryPoint point
            ++ List.map queryCoef coefs
            ++ queryObjective objective
        )


queryPoint : Variable -> UrlBuilder.QueryParameter
queryPoint (Variable name value) =
    UrlBuilder.string name (value |> String.toFloat |> Maybe.map String.fromFloat |> Maybe.withDefault "")


queryCoef : Coef -> UrlBuilder.QueryParameter
queryCoef (Coef name value) =
    UrlBuilder.string ("coef_" ++ name) (value |> String.fromFloat)


queryObjective : Maybe Objective -> List UrlBuilder.QueryParameter
queryObjective objective =
    case objective of
        Nothing ->
            []

        Just o ->
            [ UrlBuilder.string "objective" o ]


getVariables : Request.With Params -> Model -> Cmd Msg
getVariables req model =
    Http.get
        { url = queryFormula model.formula
        , expect = Http.expectJson GotVariables resultDecoder
        }


getResult : Request.With Params -> Model -> Cmd Msg
getResult req model =
    Http.get
        { url = queryResult model.formula model.initialPoint model.coefs model.selectedObjective
        , expect = Http.expectJson GotResult resultDecoder
        }



-----------
-- VIEWS --
-----------


view : Model -> View Msg
view model =
    { title = "BMO"
    , element =
        E.column [ E.centerX, E.spacing 20, E.width <| E.maximum 700 E.fill ] <|
            [ inputFormula model
            , inputObjective model
            , initialPoint model
            , nearestPoint model
            ]
    }


inputFormula : Model -> E.Element Msg
inputFormula model =
    E.column (blockAttributes ++ [ E.paddingEach { blockEdges | top = 20 } ])
        [ E.row [ E.width E.fill ]
            [ E.link []
                { url = "/"
                , label = E.text "← Back to home page"
                }
            ]
        , Input.text
            [ E.width E.fill
            , Font.color (E.rgb255 50 50 50)
            , Font.size 15
            , Font.family [ Font.monospace ]
            , Input.focusedOnLoad
            ]
            { onChange = FormulaChanged
            , text = model.formula
            , placeholder = placeholder model
            , label = Input.labelAbove [ Font.size 25 ] (E.text "Enter your formula:")
            }
        ]


inputObjective : Model -> E.Element Msg
inputObjective model =
    E.column blockAttributes
        [ E.row [] [ E.text "Select the variable corresponding to your goal" ]
        , E.row []
            [ Dropdown.view (dropdownConfig model) model model.dropdownState
                |> E.el []
            ]
        ]


placeholder : Model -> Maybe (Input.Placeholder Msg)
placeholder model =
    Just <|
        Input.placeholder []
            (E.text <|
                if model.formula == "" then
                    defaultPlaceholder

                else
                    model.formula
            )


spinnerImage : E.Element Msg
spinnerImage =
    E.image [] { src = "/public/spinner.png", description = "spinner" }


blockEdges =
    { top = 20, bottom = 50, left = 50, right = 50 }


blockAttributes : List (E.Attribute Msg)
blockAttributes =
    [ Background.color (E.rgb255 70 70 70)
    , E.spacing 20
    , E.paddingEach blockEdges
    , E.centerX
    , E.width E.fill
    , Border.rounded 15
    ]


initialPoint : Model -> E.Element Msg
initialPoint model =
    if model.spinnerV then
        E.column blockAttributes [ spinnerImage ]

    else if model.initialPoint /= [] then
        let
            newCoefs =
                Dict.union
                    (model.coefs |> List.map (\(Coef n v) -> ( n, v )) |> Dict.fromList)
                    (model.initialPoint |> List.map (\(Variable n v) -> ( "coef_" ++ n, 1.0 )) |> Dict.fromList)
                    |> Dict.toList
                    |> List.map (\( n, v ) -> Coef n v)

            maxLabelSize =
                Maybe.withDefault 0 <| List.maximum <| List.map (\(Variable n v) -> String.length n) model.initialPoint
        in
        E.column blockAttributes <|
            [ E.row [] [ E.text "Your current situation:" ] ]
                ++ List.map2
                    (\(Variable vn vv) (Coef cn cv) ->
                        E.wrappedRow [ E.spacing 50 ]
                            [ E.el [ Font.family [ Font.monospace ] ] (E.text <| String.padRight maxLabelSize ' ' vn)
                            , inputValue (Variable vn vv)

                            --, inputCoef (Coef cn cv)
                            ]
                    )
                    model.initialPoint
                    newCoefs

    else
        E.none


inputValue : Variable -> E.Element Msg
inputValue (Variable name value) =
    Input.text
        [ E.width E.fill
        , Font.color (E.rgb255 50 50 50)
        ]
        { onChange = InitialValueChanged name
        , text = value
        , placeholder = Nothing
        , label = Input.labelHidden name
        }


inputCoef : Coef -> E.Element Msg
inputCoef (Coef name coef) =
    Input.slider
        [ E.height (E.px 30)
        , E.behindContent
            (E.el
                [ E.width E.fill
                , E.height (E.px 2)
                , E.centerY
                , Background.color (E.rgb255 150 150 150)
                , Border.rounded 2
                ]
                E.none
            )
        ]
        { onChange = CoefChanged name
        , label = Input.labelAbove [] (E.text <| "Viscosity = " ++ Round.round 1 coef)
        , min = 0.0
        , max = 3.0
        , step = Just 0.1
        , value = coef
        , thumb = Input.defaultThumb
        }


displayValue : Variable -> E.Element Msg
displayValue (Variable name value) =
    E.text <|
        (value
            |> String.toFloat
            |> Maybe.map String.fromFloat
            |> Maybe.withDefault "N/A"
        )


variation : Variable -> Variable -> E.Element Msg
variation (Variable initial_name initial_value) (Variable target_name target_value) =
    case String.toFloat initial_value of
        Just initial ->
            case String.toFloat target_value of
                Just target ->
                    let
                        v =
                            (target - initial) / initial * 100

                        strv =
                            if isInfinite v then
                                "an infinity"

                            else
                                Round.round 0 v

                        direction =
                            if v > 0 then
                                "Raise "

                            else
                                "Lower "

                        colorize =
                            \x ->
                                if x >= 0 then
                                    Font.color (E.rgb255 0 200 0)

                                else
                                    Font.color (E.rgb255 200 0 0)
                    in
                    E.paragraph
                        []
                        [ E.el
                            [ colorize v ]
                            (E.text direction)
                        , E.el [ Font.bold ] (E.text target_name)
                        , E.text " by "
                        , E.el [ Font.bold, colorize v ] (E.text <| strv ++ " %")
                        ]

                Nothing ->
                    E.none

        Nothing ->
            E.none


neverFloat : Never -> Float
neverFloat =
    never


nearestPoint : Model -> E.Element Msg
nearestPoint model =
    if model.spinnerR then
        E.column blockAttributes [ spinnerImage ]

    else if model.initialPoint /= [] then
        if not <| isFilled model.initialPoint then
            E.column blockAttributes
                [ E.paragraph [] [ E.text "Fill in the initial values to compute the nearest solution" ] ]

        else
            E.column blockAttributes <|
                [ E.text "To achieve your goals you should:" ]
                    ++ List.map2
                        (\iv tv ->
                            E.row []
                                [ variation iv tv
                                , E.text " to reach a value of "
                                , displayValue tv
                                ]
                        )
                        model.initialPoint
                        model.nearestPoint

    else
        Maybe.map (\err -> E.column blockAttributes [ viewError model ]) model.error |> Maybe.withDefault E.none


notfound : Model -> E.Element Msg
notfound model =
    E.el [] (E.text "Not Found")


viewError : Model -> E.Element Msg
viewError model =
    Maybe.map
        (\t ->
            E.paragraph
                [ Font.color (E.rgb255 255 0 0)
                , Background.color (E.rgb255 255 255 255)
                , E.padding 10
                ]
                [ E.text t ]
        )
        model.error
        |> Maybe.withDefault E.none



------------------------
-- OTHER SIDE EFFECTS --
------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
