module Optim exposing
    ( Coef(..)
    , Coefs
    , Formula
    , OptimizationResult
    , Point
    , Variable(..)
    , coefsDecoder
    , pointDecoder
    , resultDecoder
    )

import Dict
import Json.Decode as Json


type alias Coefs =
    List Coef


type Coef
    = Coef String Float


type alias Point =
    List Variable


type alias Formula =
    String


type Variable
    = Variable String String


type alias OptimizationResult =
    { point : Point
    , id : Maybe String
    }


pointDecoder : Json.Decoder Point
pointDecoder =
    Json.dict (Json.maybe Json.float)
        |> Json.andThen
            (\dict ->
                Json.succeed
                    (Dict.toList dict
                        |> List.map
                            (\( k, v ) -> Variable k (Maybe.withDefault "" <| Maybe.map String.fromFloat v))
                    )
            )


coefsDecoder : Json.Decoder Coefs
coefsDecoder =
    Json.dict (Json.maybe Json.float)
        |> Json.andThen
            (\dict ->
                Json.succeed
                    (Dict.toList dict
                        |> List.map
                            (\( k, v ) -> Coef k (Maybe.withDefault 1.0 v))
                    )
            )


resultDecoder : Json.Decoder OptimizationResult
resultDecoder =
    Json.field "status" Json.string
        |> Json.andThen
            (\s ->
                if s == "success" then
                    Json.map2 OptimizationResult
                        (Json.field "point" pointDecoder)
                        (Json.field "id" (Json.maybe Json.string))

                else
                    Json.fail s
            )