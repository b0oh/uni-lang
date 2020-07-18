port module Elm exposing (main)

import Base as Opt
import Base.IO as IO exposing (IO)
import Json.Decode as Decode
import Json.Encode as Encode
import Platform
import Shell


port stdin : (Encode.Value -> msg) -> Sub msg


port stdout : Encode.Value -> Cmd msg


type Message
    = Stdin Decode.Value


type Model
    = Done
    | Running { ioState : IO.State }


run ( state, action ) =
    case action of
        IO.Done ->
            ( Done, Cmd.none )

        IO.Failed reason ->
            ( Done, stdout (Encode.string ("IO failed: " ++ reason)) )

        IO.None ->
            ( Running { ioState = state }
            , Cmd.none
            )

        IO.ReadLine ->
            ( Running { ioState = state }
            , Cmd.none
            )

        IO.Write output ->
            let
                ( nextModel, nextAction ) =
                    IO.step Opt.None state
                        |> run
            in
            ( nextModel
            , Cmd.batch [ nextAction, stdout (Encode.string output) ]
            )


init flags =
    IO.start Shell.run
        |> run


subscriptions model =
    stdin Stdin


update : Message -> Model -> ( Model, Cmd msg )
update message model =
    case model of
        Done ->
            ( model, Cmd.none )

        Running { ioState } ->
            case message of
                Stdin value ->
                    case Decode.decodeValue Decode.string value of
                        Ok decoded ->
                            let
                                input =
                                    Opt.Some (String.dropRight 1 decoded)

                                ( newModel, command ) =
                                    IO.step input ioState
                                        |> run
                            in
                            ( newModel
                            , command
                            )

                        err ->
                            ( model, Cmd.none )


main : Program () Model Message
main =
    Platform.worker
        { init = init
        , subscriptions = subscriptions
        , update = update
        }
