module Main exposing (main)

import Base
import Browser
import Browser.Events as Events
import Expr
import Html exposing (Html)
import Json.Decode as Decode
import Lambda
import Shell
import UI


type alias Model =
    { displayShell : Bool
    , env : Expr.Env
    , error : Maybe String
    , shell : Shell.State
    }


type Message
    = OnShellChange String
    | OnShellEnter
    | ToggleShell


defaultModel =
    { displayShell = True
    , env = Base.baseEnv
    , error = Nothing
    , shell = Shell.empty
    }


init : () -> ( Model, Cmd Message )
init flags =
    ( defaultModel
    , Cmd.none
    )


update : Message -> Model -> ( Model, Cmd Message )
update message ({ displayShell, shell } as model) =
    case message of
        OnShellChange command ->
            let
                newShell =
                    { shell | text = command }
            in
            ( { model | shell = newShell }
            , Cmd.none
            )

        OnShellEnter ->
            let
                newModel =
                    case Shell.run model.env model.shell of
                        Ok ( newShell, newEnv ) ->
                            { model
                                | env = newEnv
                                , error = Nothing
                                , shell = newShell
                            }

                        Err reason ->
                            { model | error = Just reason }
            in
            ( newModel
            , Cmd.none
            )

        ToggleShell ->
            ( { model | displayShell = not displayShell }
            , Cmd.none
            )


subscriptions : Model -> Sub Message
subscriptions model =
    let
        keyToMessage key =
            case key of
                "Escape" ->
                    Decode.succeed ToggleShell

                _ ->
                    Decode.fail ""

        keyDecoder =
            Decode.andThen keyToMessage (Decode.field "key" Decode.string)
    in
    Events.onKeyDown keyDecoder


view : Model -> Html Message
view { displayShell, env, error, shell } =
    let
        editorArgs =
            { env = env }

        outputArgs =
            { env = env
            , error = error
            }

        shellArgs =
            { onChange = OnShellChange
            , onEnter = OnShellEnter
            , text = shell.text
            }
    in
    UI.render
        { displayShell = displayShell
        , editor = editorArgs
        , output = outputArgs
        , shell = shellArgs
        }


main : Program () Model Message
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
