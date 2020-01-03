module Example exposing (..)

import Lambda exposing (Term(..))

zero =
    Abs "next" (Abs "init" (Var "init"))


inc =
    Abs "nat"
        (Abs "next"
             (Abs "init"
                  (App
                      (Var "next")
                      (App
                          (App
                              (Var "nat")
                              (Var "next")
                          )
                          (Var "init")
                      )
                  )
             )
        )
