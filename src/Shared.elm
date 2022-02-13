module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    )

import Json.Decode as Json
import Optim exposing (..)
import Request exposing (Request)


type alias Flags =
    Json.Value


type alias Initval =
    { formula : Formula
    , initialPoint : Point
    , coefs : Coefs

    --    , closest_solution : Point
    }


flagsDecoder : Json.Decoder Initval
flagsDecoder =
    Json.map3 Initval
        (Json.field "formula" Json.string)
        (Json.field "initial_point" pointDecoder)
        (Json.field "coefs" coefsDecoder)


type alias Model =
    { initval : Initval }


type Msg
    = NoOp


init : Request -> Json.Value -> ( Model, Cmd Msg )
init req flags =
    let
        initval =
            case Json.decodeValue flagsDecoder flags of
                Ok f ->
                    f

                Err error ->
                    { formula = "", initialPoint = [], coefs = [] }
    in
    ( { initval = initval }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
