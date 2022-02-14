module Shared exposing
    ( Flags
    , Model
    , Msg
    , WindowSize
    , init
    , subscriptions
    , update
    )

import Browser.Events as Events
import Json.Decode as Json
import Optim exposing (Coefs, Formula, Point, coefsDecoder, pointDecoder)
import Request exposing (Request)


type alias Flags =
    Json.Value


type alias WindowSize =
    { w : Int
    , h : Int
    }


type alias Model =
    { formula : Formula
    , initialPoint : Point
    , coefs : Coefs
    , windowSize : WindowSize
    }


flagsDecoder : Json.Decoder Model
flagsDecoder =
    Json.map4 Model
        (Json.field "formula" Json.string)
        (Json.field "initial_point" pointDecoder)
        (Json.field "coefs" coefsDecoder)
        (Json.field "window_size"
            (Json.map2 WindowSize (Json.field "w" Json.int) (Json.field "h" Json.int))
        )


type Msg
    = WindowResized WindowSize


init : Request -> Json.Value -> ( Model, Cmd Msg )
init _ flags =
    let
        model =
            case Json.decodeValue flagsDecoder flags of
                Ok m ->
                    m

                Err _ ->
                    { formula = ""
                    , initialPoint = []
                    , coefs = []
                    , windowSize = WindowSize 1024 768
                    }
    in
    ( model
    , Cmd.none
    )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        WindowResized size ->
            ( { model | windowSize = size }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Events.onResize (\width height -> WindowResized (WindowSize width height))
