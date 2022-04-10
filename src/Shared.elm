module Shared exposing (Flags, Model, Msg, WindowSize, identity, init, subscriptions, update)

import Browser.Events as Events
import Browser.Navigation as Nav
import Json.Decode as Json
import Optim exposing (Coefs, Formula, Point, coefsDecoder, pointDecoder)


type alias WindowSize =
    { w : Int
    , h : Int
    }


type alias Flags =
    { formula : Formula
    , initialPoint : Point
    , coefs : Coefs
    , nbsimu : Int
    , windowSize : WindowSize
    }


type alias Model =
    { key : Nav.Key
    , identity : Maybe String
    , formula : Formula
    , initialPoint : Point
    , coefs : Coefs
    , nbsimu : Int
    , windowSize : WindowSize
    }


identity : Model -> Maybe String
identity =
    .identity


flagsDecoder : Json.Decoder Flags
flagsDecoder =
    Json.map5 Flags
        (Json.field "formula" Json.string)
        (Json.field "initial_point" pointDecoder)
        (Json.field "coefs" coefsDecoder)
        (Json.field "nbsimu" Json.int)
        (Json.field "window_size"
            (Json.map2 WindowSize (Json.field "w" Json.int) (Json.field "h" Json.int))
        )


type Msg
    = WindowResized WindowSize


init : Json.Value -> Nav.Key -> ( Model, Cmd Msg )
init flags key =
    let
        model =
            case Json.decodeValue flagsDecoder flags of
                Ok f ->
                    { key = key
                    , identity = Nothing
                    , formula = f.formula
                    , initialPoint = f.initialPoint
                    , coefs = f.coefs
                    , nbsimu = f.nbsimu
                    , windowSize = f.windowSize
                    }

                Err _ ->
                    { key = key
                    , identity = Nothing
                    , formula = ""
                    , initialPoint = []
                    , coefs = []
                    , nbsimu = 0
                    , windowSize = WindowSize 1024 768
                    }
    in
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized size ->
            ( { model | windowSize = size }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.onResize (\width height -> WindowResized (WindowSize width height))
