module Pages.App exposing (Model, Msg, page)

--import Dropdown

import Browser.Navigation as Nav
import Debouncer.Basic as Debouncer exposing (Debouncer)
import Dict
import Effect exposing (Effect)
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Http
import Optim exposing (Coef(..), Coefs, Formula, OptimizationResult, Point, Variable(..), isFilled, resultDecoder)
import Round
import Route exposing (OptimFlags(..), formulaUrl, idUrl)
import Shared
import Spa.Page
import Task
import Url.Builder as UrlBuilder
import View exposing (View)


page : Shared.Model -> Spa.Page.Page OptimFlags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init shared
        , update = update shared
        , view = view
        , subscriptions = subscriptions
        }
        |> Spa.Page.onNewFlags (\_ -> Void)



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
    , debouncer : Debouncer Msg Msg
    , spinnerV : Bool
    , spinnerR : Bool
    , error : Maybe String

    --, dropdownState : Dropdown.State Objective
    , selectedObjective : Maybe Objective
    }


type Msg
    = Void
    | FormulaChanged String
    | GetVariables
    | GotVariables (Result Http.Error OptimizationResult)
    | InitialValueChanged String String
      --    | CoefChanged String Float
    | GetResult
    | GotResult (Result Http.Error OptimizationResult)
    | Debounce (Debouncer.Msg Msg)



--| OptionPicked (Maybe Objective)
--| DropdownMsg (Dropdown.Msg Objective)
----------
-- INIT --
----------


defaultPlaceholder : String
defaultPlaceholder =
    ""


init : Shared.Model -> OptimFlags -> ( Model, Effect Shared.Msg Msg )
init shared flags =
    let
        formula =
            case flags of
                OQFormula f ->
                    f

                _ ->
                    shared.formula

        model =
            { formula = formula
            , initialPoint = shared.initialPoint
            , coefs = shared.coefs
            , nearestPoint = []
            , debouncer = Debouncer.manual |> Debouncer.settleWhenQuietFor (Just <| Debouncer.fromSeconds 1.5) |> Debouncer.toDebouncer
            , spinnerV = formula /= ""
            , spinnerR = False
            , error = Nothing

            --, dropdownState = Dropdown.init "Select your objective"
            , selectedObjective = Nothing
            }
    in
    ( model, Effect.fromCmd <| getVariables model )



--pointToNames : Point -> List String
--pointToNames point =
--    List.map (\(Variable n _) -> n) point
--
--
--
--dropdownConfig : Model -> Dropdown.Config Objective Msg Model
--dropdownConfig model =
--    let
--        containerAttrs =
--            [ E.width (E.px 300) ]
--
--        selectAttrs =
--            [ Border.width 1, Border.rounded 5, E.paddingXY 16 8, E.spacing 10, E.width E.fill ]
--
--        searchAttrs =
--            [ Border.width 0, E.padding 0 ]
--
--        listAttrs =
--            [ Border.width 1
--            , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
--            , E.width E.fill
--            , E.spacing 5
--            ]
--
--        itemToPrompt item =
--            E.text item
--
--        itemToElement selected highlighted i =
--            let
--                bgColor =
--                    if highlighted then
--                        E.rgb255 70 70 70
--
--                    else if selected then
--                        E.rgb255 80 80 80
--
--                    else
--                        E.rgb255 50 50 50
--            in
--            E.row
--                [ Background.color bgColor
--                , E.padding 8
--                , E.spacing 10
--                , E.width E.fill
--                ]
--                [ E.el [] (E.text "-")
--                , E.el [ Font.size 16 ] (E.text i)
--                ]
--    in
--    Dropdown.filterable
--        { itemsFromModel = always <| pointToNames model.initialPoint
--        , selectionFromModel = .selectedObjective
--
--        --, dropdownMsg = DropdownMsg
--        --, onSelectMsg = OptionPicked
--        , itemToPrompt = itemToPrompt
--        , itemToElement = itemToElement
--        , itemToText = identity
--        }
--        |> Dropdown.withContainerAttributes containerAttrs
--        |> Dropdown.withPromptElement (E.el [] (E.text "Select option"))
--        |> Dropdown.withFilterPlaceholder "Type for option"
--        |> Dropdown.withSelectAttributes selectAttrs
--        |> Dropdown.withListAttributes listAttrs
--        |> Dropdown.withSearchAttributes searchAttrs
------------
-- UPDATE --
------------


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        Void ->
            ( model, Effect.none )

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
            , Effect.perform
                (Debounce << Debouncer.provideInput << (\_ -> GetVariables))
                (Task.succeed "")
            )

        GetVariables ->
            ( { model | spinnerV = True }
            , Effect.fromCmd <|
                Cmd.batch
                    [ getVariables model
                    , Nav.replaceUrl shared.key <| formulaUrl model.formula
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
                            (\(Coef n _) b ->
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
                , Effect.perform ((\_ -> GetResult) >> Debouncer.provideInput >> Debounce) (Task.succeed "")
                )

            else
                ( { model | initialPoint = newpoint, coefs = newcoefs, nearestPoint = [] }
                , Effect.none
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
                        Effect.fromCmd <| getResult model

                      else
                        Effect.none
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
                    , Effect.none
                    )

        GetResult ->
            ( { model | spinnerR = True }, Effect.fromCmd <| getResult model )

        GotResult optresult ->
            case optresult of
                Ok result ->
                    ( { model | nearestPoint = result.point, spinnerR = False }
                    , Effect.fromCmd <| Nav.pushUrl shared.key <| idUrl (Maybe.withDefault "" result.id)
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
                    , Effect.none
                    )

        Debounce debMsg ->
            let
                ( debouncer, subCmd, emittedMsg ) =
                    Debouncer.update debMsg model.debouncer

                mappedCmd =
                    Effect.map Debounce (Effect.fromCmd subCmd)

                updatedModel =
                    { model | debouncer = debouncer }
            in
            case emittedMsg of
                Just emitted ->
                    update shared emitted updatedModel
                        |> Tuple.mapSecond (\cmd -> Effect.batch [ cmd, mappedCmd ])

                Nothing ->
                    ( updatedModel, mappedCmd )



--        CoefChanged name coef ->
--            let
--                newcoefs =
--                    List.map
--                        (\(Coef n v) ->
--                            if name == n then
--                                Coef n coef
--
--                            else
--                                Coef n v
--                        )
--                        model.coefs
--            in
--            if isFilled model.initialPoint then
--                ( { model | coefs = newcoefs, spinnerR = True }
--                , Task.perform ((\_ -> GetResult) >> Debouncer.provideInput >> Delay) (Task.succeed "")
--                )
--
--            else
--                ( { model | coefs = newcoefs, nearestPoint = [] }
--                , Cmd.none
--                )
--OptionPicked option ->
--    ( { model | selectedObjective = option }
--    , Task.perform ((\_ -> GetResult) >> Debouncer.provideInput >> Delay) (Task.succeed "")
--    )
--DropdownMsg subMsg ->
--    let
--        ( state, cmd ) =
--            Dropdown.update (dropdownConfig model) subMsg model model.dropdownState
--    in
--    ( { model | dropdownState = state }, cmd )


httpErrorToString : Http.Error -> Maybe String
httpErrorToString error =
    case error of
        Http.BadBody err ->
            err |> String.split "\n" |> List.reverse |> List.head

        _ ->
            Just "Other error"


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
            (\(Variable k _) ->
                Variable k
                    (Dict.get k existingvalues |> Maybe.withDefault "")
            )



---------------------
-- TALK TO BACKEND --
---------------------


qsFormula : String -> String
qsFormula inputstring =
    UrlBuilder.absolute [ "optimize" ] [ UrlBuilder.string "formula" inputstring ]


qsResult : Formula -> Point -> Coefs -> Maybe Objective -> String
qsResult inputstring point coefs objective =
    UrlBuilder.absolute [ "optimize" ]
        (UrlBuilder.string "formula" inputstring
            :: List.map qsPoint point
            ++ List.map qsCoef coefs
            ++ qsObjective objective
        )


qsPoint : Variable -> UrlBuilder.QueryParameter
qsPoint (Variable name value) =
    UrlBuilder.string name (value |> String.toFloat |> Maybe.map String.fromFloat |> Maybe.withDefault "")


qsCoef : Coef -> UrlBuilder.QueryParameter
qsCoef (Coef name value) =
    UrlBuilder.string ("coef_" ++ name) (value |> String.fromFloat)


qsObjective : Maybe Objective -> List UrlBuilder.QueryParameter
qsObjective objective =
    case objective of
        Nothing ->
            []

        Just o ->
            [ UrlBuilder.string "objective" o ]


getVariables : Model -> Cmd Msg
getVariables model =
    Http.get
        { url = qsFormula model.formula
        , expect = Http.expectJson GotVariables resultDecoder
        }


getResult : Model -> Cmd Msg
getResult model =
    Http.get
        { url = qsResult model.formula model.initialPoint model.coefs model.selectedObjective
        , expect = Http.expectJson GotResult resultDecoder
        }



-----------
-- VIEWS --
-----------


view : Model -> View Msg
view model =
    { title = "BMO"
    , attributes =
        [ Background.color (E.rgb255 42 44 43)
        , Font.color (E.rgb255 217 203 158)
        , E.paddingEach { top = 10, right = 0, bottom = 100, left = 0 }
        ]
    , element =
        E.column [ E.centerX, E.spacing 20, E.width <| E.maximum 700 E.fill ] <|
            [ inputFormula model

            --, inputObjective model
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
        , E.column [ E.width E.fill ]
            [ E.text "Click on these examples:"
            , E.link [ E.paddingXY 16 8, Font.bold, Events.onClick <| FormulaChanged "results = revenue - expenses" ]
                { url = "/app?formula=result%20%3D%20revenue%20-%20expenses"
                , label = E.text "→ results = revenue - expenses"
                }
            , E.link [ E.paddingXY 16 8, Font.bold, Events.onClick <| FormulaChanged "price1 * NbSales1 + price2 * NbSales2 - expenses = 100000" ]
                { url = "/app?formula=price1%20*%20NbSales1%20%2B%20price2%20*%20NbSales2%20-%20expenses%20%3D%20100000"
                , label = E.text "→ price1 * NbSales1 + price2 * NbSales2 - expenses = 100000"
                }
            ]
        ]



--inputObjective : Model -> E.Element Msg
--inputObjective model =
--    E.column blockAttributes
--        [ E.row [] [ E.text "Select the variable corresponding to your goal" ]
--        , E.row []
--            [ Dropdown.view (dropdownConfig model) model model.dropdownState
--                |> E.el []
--            ]
--        ]


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


blockEdges : { top : Int, bottom : Int, left : Int, right : Int }
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
                    (model.initialPoint |> List.map (\(Variable n _) -> ( "coef_" ++ n, 1.0 )) |> Dict.fromList)
                    |> Dict.toList
                    |> List.map (\( n, v ) -> Coef n v)

            maxLabelSize =
                Maybe.withDefault 0 <| List.maximum <| List.map (\(Variable n _) -> String.length n) model.initialPoint
        in
        E.column blockAttributes <|
            E.row [] [ E.text "Your current situation:" ]
                :: List.map2
                    (\(Variable vn vv) _ ->
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



--inputCoef : Coef -> E.Element Msg
--inputCoef (Coef name coef) =
--    Input.slider
--        [ E.height (E.px 30)
--        , E.behindContent
--            (E.el
--                [ E.width E.fill
--                , E.height (E.px 2)
--                , E.centerY
--                , Background.color (E.rgb255 150 150 150)
--                , Border.rounded 2
--                ]
--                E.none
--            )
--        ]
--        { onChange = CoefChanged name
--        , label = Input.labelAbove [] (E.text <| "Viscosity = " ++ Round.round 1 coef)
--        , min = 0.0
--        , max = 3.0
--        , step = Just 0.1
--        , value = coef
--        , thumb = Input.defaultThumb
--        }


displayValue : Variable -> E.Element Msg
displayValue (Variable _ value) =
    E.text <|
        (value
            |> String.toFloat
            |> Maybe.map String.fromFloat
            |> Maybe.withDefault "N/A"
        )


variation : Variable -> Variable -> E.Element Msg
variation (Variable _ initial_value) (Variable target_name target_value) =
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
                E.text "To achieve your goals you should:"
                    :: List.map2
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
        viewError model.error


viewError : Maybe String -> E.Element Msg
viewError error =
    Maybe.map
        (\err ->
            E.column blockAttributes
                [ E.paragraph
                    [ Font.color (E.rgb255 255 0 0)
                    , Background.color (E.rgb255 255 255 255)
                    , E.padding 10
                    ]
                    [ E.text err ]
                ]
        )
        error
        |> Maybe.withDefault E.none



------------------------
-- OTHER SIDE EFFECTS --
------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
