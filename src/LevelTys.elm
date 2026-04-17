module LevelTys exposing (..)

import Expr exposing (Expr)
import Set exposing (Set)


type Reason
    = Monotony Int
    | Hypotesis
    | Equivalence { name : String, ref : Int }
    | Implication { name : String, ref : Int }
    | InferenceRule1 { name : String, ref1 : Int }
    | InferenceRule2 { name : String, ref1 : Int, ref2 : Int }


type Step
    = Assume (Maybe Expr)
    | Deduction { assumed : Maybe Expr, num : Int, what : Expr, reason : Reason }


type alias LevelModelEx =
    { descr : String
    , goal : Expr
    , ded_text : String
    , error_msg : Maybe String
    , theory : List Expr
    , steps : List Step
    , known_props : Set String
    }


type LevelModel
    = Ex LevelModelEx


type LevelMsg
    = DeductionTextChanged String
    | AddPressed
    | TheoryPressed
    | ExprPressed Expr
