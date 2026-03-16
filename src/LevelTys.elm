module LevelTys exposing (..)

import Expr exposing (Expr)


type Reason
    = Monotony Int
    | Hypotesis
    | Equivalence { number : Int, ref : Int }
    | Implication { number : Int, ref : Int }
    | InferenceRule1 { number : Int, ref1 : Int }
    | InferenceRule2 { number : Int, ref1 : Int, ref2 : Int }


type Step
    = Assume (Maybe Expr)
    | Deduction { assumed : Maybe Expr, num : Int, what : Expr, reason : Reason }


type LevelModel
    = Ex { ded_text : String, error_msg : Maybe String, theory : List Expr, steps : List Step }


type LevelMsg
    = DeductionTextChanged String
    | AddPressed
    | TheoryPressed
    | ExprPressed Expr
