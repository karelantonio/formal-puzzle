module Infer.InferenceRule exposing (tryFromInferenceRule)

import AllRules exposing (allInferenceRulesOne)
import Dict
import Expr.Types exposing (Expr)
import Infer.Types exposing (InferenceRefs(..))
import List exposing (head)
import Match2 exposing (emptyClues, matchAllTwice, replace)
import Utils exposing (listProduct)


tryFromInferenceRule : List { number : Int, assum : Maybe Expr, what : Expr } -> Maybe Expr -> Expr -> Maybe { refs : InferenceRefs, name : String }
tryFromInferenceRule steps hyp res =
    case hyp of
        Just val ->
            tryFromInferenceRuleNonAssuming steps (Just val) res

        Nothing ->
            case tryFromInferenceRuleAssuming steps res of
                Just val ->
                    Just val

                Nothing ->
                    tryFromInferenceRuleNonAssuming steps Nothing res


tryFromInferenceRuleAssuming : List { number : Int, assum : Maybe Expr, what : Expr } -> Expr -> Maybe { refs : InferenceRefs, name : String }
tryFromInferenceRuleAssuming lst ex =
    List.filterMap
        (tryFromInferenceRuleAssumingCheck ex)
        (listProduct lst AllRules.allInferenceRulesAssuming)
        |> List.head


tryFromInferenceRuleAssumingCheck : Expr -> ( { number : Int, assum : Maybe Expr, what : Expr }, { name : String, assum : Expr, what : Expr, thesis : Expr } ) -> Maybe { name : String, refs : InferenceRefs }
tryFromInferenceRuleAssumingCheck ex ( oex, rule ) =
    case oex.assum of
        Just assum ->
            matchAllTwice [ assum, oex.what, ex ] [ rule.assum, rule.what, rule.thesis ] emptyClues
                |> Result.andThen (replace rule.thesis)
                |> Result.andThen
                    (\e ->
                        if e == ex then
                            Ok { name = rule.name, refs = OneRef oex.number }

                        else
                            Err ()
                    )
                |> Result.toMaybe

        _ ->
            Nothing


tryFromInferenceRuleNonAssuming : List { number : Int, assum : Maybe Expr, what : Expr } -> Maybe Expr -> Expr -> Maybe { refs : InferenceRefs, name : String }
tryFromInferenceRuleNonAssuming lst ass ex =
    -- Filter by the same theory
    let
        theo =
            List.filterMap (tryFromInferenceFilterSameTheory ass) lst
    in
    case
        List.filterMap
            (tryFromInferenceFilterMatch1 ex)
            (listProduct theo allInferenceRulesOne)
            |> head
    of
        Just v ->
            Just v

        Nothing ->
            List.filterMap
                (tryFromInferenceFilterMatch2 ex)
                (listProduct theo (listProduct theo AllRules.allInferenceRulesTwo))
                |> head


tryFromInferenceFilterSameTheory : Maybe Expr -> { number : Int, assum : Maybe Expr, what : Expr } -> Maybe { number : Int, what : Expr }
tryFromInferenceFilterSameTheory orig e =
    if orig == e.assum then
        Just { number = e.number, what = e.what }

    else
        Nothing


tryFromInferenceFilterMatch1 :
    Expr
    -> ( { number : Int, what : Expr }, { name : String, what : Expr, thesis : Expr } )
    -> Maybe { refs : InferenceRefs, name : String }
tryFromInferenceFilterMatch1 ex ( oex, pat ) =
    matchAllTwice [ oex.what, ex ] [ pat.what, pat.thesis ] emptyClues
        |> Result.andThen (replace pat.thesis)
        |> Result.andThen
            (\e ->
                if e == ex then
                    Ok { name = pat.name, refs = OneRef oex.number }

                else
                    Err ()
            )
        |> Result.toMaybe


tryFromInferenceFilterMatch2 :
    Expr
    -> ( { number : Int, what : Expr }, ( { number : Int, what : Expr }, { name : String, what1 : Expr, what2 : Expr, thesis : Expr } ) )
    -> Maybe { refs : InferenceRefs, name : String }
tryFromInferenceFilterMatch2 ex ( e1, ( e2, pat ) ) =
    matchAllTwice [ e1.what, e2.what, ex ] [ pat.what1, pat.what2, pat.thesis ] emptyClues
        |> Result.andThen (replace pat.thesis)
        |> Result.andThen
            (\e ->
                if e == ex then
                    Ok { name = pat.name, refs = TwoRefs e1.number e2.number }

                else
                    Err ()
            )
        |> Result.toMaybe
