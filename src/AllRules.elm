module AllRules exposing (allEquivalences, allImplications, allInferenceRulesAssuming, allInferenceRulesOne, allInferenceRulesTwo)

{-| All the rules
-}

import Dict
import Expr.Parser exposing (parse)
import Expr.Types exposing (Domain, Expr(..), emptyDomain)
import Set


allEquivalences : List ( String, Expr, Expr )
allEquivalences =
    [ pr "L1" "A|A" "A"
    , pr "L2" "A&A" "A"
    , pr "L3" "(A|B)|C" "A|(B|C)"
    , pr "L4" "(A&B)&C" "A&(B&C)"
    , pr "L5" "A|B" "B|A"
    , pr "L6" "A&B" "B&A"
    , pr "L7" "A|(B&C)" "(A|B)&(A|C)"
    , pr "L8" "A&(B|C)" "(A&B)|(A&C)"
    , pr "L9" "A|(A&B)" "A"
    , pr "L10" "A&(A|B)" "A"
    , pr "L11" "A|0" "A"
    , pr "L12" "A&1" "A"
    , pr "L13" "A|1" "1"
    , pr "L14" "A&0" "0"
    , pr "L15" "A|-A" "1"
    , pr "L16" "A&-A" "0"
    , pr "L16" "-A&A" "0"
    , pr "L17" "--A" "A"
    , pr "L18" "-1" "0"
    , pr "L19" "-0" "1"
    , pr "L20" "-(A|B)" "-A&-B"
    , pr "L21" "-(A&B)" "-A|-B"
    , pr "L22" "A->B" "-A|B"
    , pr "L22" "-A->B" "A|B"
    , pr "L23" "A<->B" "(A->B)&(B->A)"
    , pr "L43" "∀(x)[A(x)->B]" "∃(x)A(x)->B"
    , pr "L44" "∀(x)-A(x)" "-(∃(x)A(x))"

    -- TODO: Add the remaining rules
    ]


allImplications : List ( String, Expr, Expr )
allImplications =
    [ pr "L24" "A" "B->(A&B)"
    , pr "L25" "(A->B)&(B->C)" "A->C"
    , pr "L26" "-B->-A" "A->B"
    , pr "L26" "B->-A" "A->-B"
    , pr "L26" "-B->A" "-A->B"
    , pr "L26" "-B->A" "-A->B"
    , pr "L27" "(A->C)&(B->C)" "(A|B)->C"
    , pr "L28" "(A->B)&(-A->B)" "B"
    , pr "L29" "-A->(B&-B)" "A"
    , pr "L30" "A->(B&-B)" "-A"
    , pr "L31" "-A->A" "A"
    , pr "L32" "A->-A" "-A"
    , pr "L33" "A->(-B->(C&-C))" "A->B"
    , pr "L34" "(A&-B)->B" "A->B"
    , pr "L35" "(A&-B)->-A" "A->B"
    , pr "L36" "(A->B)&(-A->C)" "B|C"
    , pr "L37" "(-A->B)" "A|B"
    , pr "L38" "A->B" "B|-A"
    , pr "L39" "A" "A|B"
    , pr "L40" "A&B" "A"
    , pr "L41" "A" "B->A"
    , pr "L42" "∀(x)A(x)" "A(t)"
    ]


pr : String -> String -> String -> ( String, Expr, Expr )
pr nm s1 s2 =
    ( nm, parseAndUnwrap s1, parseAndUnwrap s2 )


allInferenceRulesAssuming : List { name : String, assum : Expr, what : Expr, thesis : Expr }
allInferenceRulesAssuming =
    [ assuming "RI2" "A" "B" "A->B"
    , assuming "RI9" "-A" "B&-B" "A"
    , assuming "RI9" "A" "B&-B" "-A"
    , assuming "RI9" "-A" "-B&B" "A"
    , assuming "RI9" "A" "-B&B" "-A"
    , assuming "RI9" "A&-B" "C&-C" "A->B"
    , assuming "RI9" "A&-B" "B" "A->B"
    , assuming "RI9" "A&-B" "-A" "A->B"
    ]


allInferenceRulesOne : List { name : String, what : Expr, thesis : Expr }
allInferenceRulesOne =
    [ steps1 "RI4" "A&B" "A"
    , steps1 "RI4" "A&B" "B"
    , steps1 "RI5" "A" "A|B"
    , steps1 "RI5" "A" "B|A"
    , steps1 "RI7" "-B->-A" "A->B"
    , steps1 "RI7" "B->A" "-A->-B"
    , steps1 "RI10" "-A->B" "A|B"
    , steps1 "RI10" "A->B" "-A|B"
    ]


allInferenceRulesTwo : List { name : String, what1 : Expr, what2 : Expr, thesis : Expr }
allInferenceRulesTwo =
    [ steps2 "RI1" "A" "A->B" "B"
    , steps2 "RI3" "A" "B" "A&B"
    , steps2 "RI6" "A->C" "B->C" "(A|B)->C"
    , steps2 "RI6" "-A->B" "A->B" "B"
    , steps2 "RI8" "A->C" "C->B" "A->B"
    , steps2 "RI10" "A->B" "-A->C" "B|C"
    , steps2 "RI11" "A->B" "B->A" "A<->B"
    ]


assuming : String -> String -> String -> String -> { name : String, assum : Expr, what : Expr, thesis : Expr }
assuming name hy what th =
    { name = name, assum = parseAndUnwrap hy, what = parseAndUnwrap what, thesis = parseAndUnwrap th }


steps1 : String -> String -> String -> { name : String, what : Expr, thesis : Expr }
steps1 name what th =
    { name = name, what = parseAndUnwrap what, thesis = parseAndUnwrap th }


steps2 : String -> String -> String -> String -> { name : String, what1 : Expr, what2 : Expr, thesis : Expr }
steps2 name what1 what2 th =
    { name = name, what1 = parseAndUnwrap what1, what2 = parseAndUnwrap what2, thesis = parseAndUnwrap th }


parseAndUnwrap : String -> Expr
parseAndUnwrap s =
    case parse commonDomain s of
        Ok res ->
            res

        Err _ ->
            -- Workaround, but should reach here
            -- todo ("Not implemented (" ++ Debug.toString err ++ ")")
            Ident "[redacted]"


commonDomain : Domain
commonDomain =
    { domain = Set.singleton "t"
    , functions = Dict.singleton "f" 1
    , predicates = Dict.fromList [ ( "A", 1 ), ( "B", 1 ), ( "C", 1 ), ( "A2", 2 ), ( "B2", 2 ), ( "C2", 2 ), ( "P", 1 ), ( "Q", 1 ), ( "R", 1 ), ( "P2", 2 ), ( "Q2", 2 ), ( "R2", 2 ) ]
    , propositions = Set.fromList [ "A", "B", "C", "D" ]
    }
