module Level.Types exposing (..)

import Expr.Types exposing (Domain, Expr(..), extractDomainFromTheory)
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


type DescrItem
    = Text String
    | Theory Expr


type alias ExT =
    { descr : List DescrItem
    , goal : Expr
    , ded_text : String
    , error_msg : Maybe String
    , theory : List Expr
    , domain : Domain
    , steps : List Step
    }


{-| Usen in AllLevels to create the levels
-}
makeLevel : { descr : List DescrItem, goal : Expr } -> Model
makeLevel info =
    let
        theory =
            List.filterMap
                (\e ->
                    case e of
                        Theory i ->
                            Just i

                        _ ->
                            Nothing
                )
                info.descr
    in
    Ex
        { descr = info.descr
        , goal = info.goal
        , theory = theory
        , ded_text = ""
        , error_msg = Nothing
        , steps = [ Assume Nothing ]
        , domain = extractDomainFromTheory (info.goal :: theory)
        }


type Model
    = Ex ExT


type Msg
    = DeductionTextChanged String
    | AddPressed
    | TheoryPressed
    | ExprPressed Expr
    | InsertPressed String
