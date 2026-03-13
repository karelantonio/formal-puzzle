module AllRules exposing (allEquivalences, allImplications)

{-| All the rules
-}

import Debug exposing (todo)
import Expr exposing (..)


allEquivalences : List ( Expr, Expr )
allEquivalences =
    [ pr "A|A" "A"
    , pr "A&A" "A"
    , pr "(A|B)|C" "A|(B|C)"
    , pr "(A&B)&C" "A&(B&C)"
    , pr "A|B" "B|A"
    , pr "A&B" "B&A"
    , pr "A|(B&C)" "(A|B)&(A|C)"
    , pr "A&(B|C)" "(A&B)|(A&C)"
    , pr "A|(A&B)" "A"
    , pr "A&(A|B)" "A"
    , pr "A|0" "A"
    , pr "A&1" "A"
    , pr "A|1" "1"
    , pr "A&0" "0"
    , pr "A|-A" "1"
    , pr "A&-A" "0"
    , pr "--A" "A"
    , pr "-1" "0"
    , pr "-0" "1"
    , pr "-(A|B)" "-A&-B"
    , pr "-(A&B)" "-A|-B"
    , pr "A->B" "-A|B"
    , pr "A<->B" "(A->B)&(B->A)"
    ]


allImplications : List ( Expr, Expr )
allImplications =
    [ pr "A" "B->(A&B)"
    , pr "(A->B)&(B->C)" "A->C"
    ]


pr : String -> String -> ( Expr, Expr )
pr s1 s2 =
    ( parseAndUnwrap s1, parseAndUnwrap s2 )


parseAndUnwrap : String -> Expr
parseAndUnwrap s =
    case parse s of
        Ok res ->
            res

        Err err ->
            todo ("Not implemented (" ++ Debug.toString err ++ ")")
