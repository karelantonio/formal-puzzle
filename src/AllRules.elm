module AllRules exposing (allEquivalences, allImplications, allInferenceRulesAssuming, allInferenceRulesOne, allInferenceRulesTwo)

{-| All the rules
-}

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
    , pr "-B->-A" "A->B"
    , pr "(A->C)&(B->C)" "(A|B)->C"
    , pr "(A->B)&(-A->B)" "B"
    , pr "-A->(B&-B)" "A"
    , pr "A->(B&-B)" "-A"
    , pr "-A->A" "A"
    , pr "A->-A" "-A"
    , pr "A->(-B->(C&-C))" "A->B"
    , pr "(A&-B)->B" "A->B"
    , pr "(A&-B)->-A" "A->B"
    , pr "(A->B)&(-A->C)" "B|C"
    , pr "(-A->B)" "A|B"
    , pr "A->B" "B|-A"
    , pr "A" "A|B"
    , pr "A&B" "A"
    , pr "A" "B->A"
    ]


pr : String -> String -> ( Expr, Expr )
pr s1 s2 =
    ( parseAndUnwrap s1, parseAndUnwrap s2 )


allInferenceRulesAssuming : List { assum : Expr, what : Expr, thesis : Expr }
allInferenceRulesAssuming =
    [ assuming "A" "B" "A->B"
    , assuming "-A" "B&-B" "A"
    , assuming "A" "B&-B" "-A"
    , assuming "A&-B" "C&-C" "A->B"
    , assuming "A&-B" "B" "A->B"
    , assuming "A&-B" "-A" "A->B"
    ]


allInferenceRulesOne : List { what : Expr, thesis : Expr }
allInferenceRulesOne =
    [ steps1 "A&B" "A"
    , steps1 "A&B" "B"
    , steps1 "A" "A|B"
    , steps1 "A" "B|A"
    , steps1 "-B->-A" "A->B"
    , steps1 "-A->B" "A|B"
    , steps1 "A->B" "-A|B"
    ]


allInferenceRulesTwo : List { what1 : Expr, what2 : Expr, thesis : Expr }
allInferenceRulesTwo =
    [ steps2 "A" "A->B" "B"
    , steps2 "A->B" "B->A" "A<->B"
    , steps2 "A" "B" "A&B"
    , steps2 "A->C" "B->C" "(A|B)->C"
    , steps2 "-A->B" "A->B" "B"
    , steps2 "A->C" "C->B" "A->B"
    , steps2 "A->B" "-A->C" "B|C"
    ]


assuming : String -> String -> String -> { assum : Expr, what : Expr, thesis : Expr }
assuming hy what th =
    { assum = parseAndUnwrap hy, what = parseAndUnwrap what, thesis = parseAndUnwrap th }


steps1 : String -> String -> { what : Expr, thesis : Expr }
steps1 what th =
    { what = parseAndUnwrap what, thesis = parseAndUnwrap th }


steps2 : String -> String -> String -> { what1 : Expr, what2 : Expr, thesis : Expr }
steps2 what1 what2 th =
    { what1 = parseAndUnwrap what1, what2 = parseAndUnwrap what2, thesis = parseAndUnwrap th }


parseAndUnwrap : String -> Expr
parseAndUnwrap s =
    case parse s of
        Ok res ->
            res

        Err err ->
            -- Workaround, but should reach here
            -- todo ("Not implemented (" ++ Debug.toString err ++ ")")
            Ident "[redacted]"
