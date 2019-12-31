module Shell exposing (State, empty, run)

import Dict exposing (Dict)
import Do.Result as Result
import Expr
import Sexp
import Scheme


type alias State =
    { text : String
    , visible : Bool
    }


empty =
    { text = ""
    , visible = False
    }


run : Expr.Env -> State -> Result String ( State, Expr.Env )
run env0 state =
    let
        result =
            state.text
                |> Sexp.fromString
                |> Result.andThen Scheme.fromSexp
                |> Result.andThen (Expr.fromLambda >> Expr.eval env0)
    in
    Result.do result <| \( newAcc, env1 ) ->
    Ok ( { state | text = "" }, Dict.insert "acc" newAcc env1 )
