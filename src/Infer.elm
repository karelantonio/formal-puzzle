module Infer exposing (InferenceRefs(..), Transformation(..), tryFromHypothesis, tryFromImplication, tryFromInferenceRule, tryFromMonotony, tryFromReplacement, wasApplied)

{-| Check if this some expressions can be inferred from others
-}

import AllRules exposing (..)
import Dict
import Expr exposing (..)
import List exposing (head)
import Match exposing (..)
import Utils exposing (listProduct)


{-| A transformation
-}
type Transformation
    = Replacement Expr Expr
    | LogicalImplication Expr Expr


{-| Check if the second expression can be inferred from the first one using the given transformation
-}
wasApplied : Transformation -> Expr -> Expr -> Bool
wasApplied trans e1 e2 =
    case trans of
        Replacement a b ->
            wasAppliedRepl ( a, b ) ( e1, e2 ) || wasAppliedRepl ( b, a ) ( e1, e2 )

        LogicalImplication a b ->
            wasLiterallyAppliedReplHere ( a, b ) ( e1, e2 )


wasAppliedRepl : ( Expr, Expr ) -> ( Expr, Expr ) -> Bool
wasAppliedRepl ( p1, p2 ) ( e1, e2 ) =
    -- Was applied right here?
    if wasLiterallyAppliedReplHere ( p1, p2 ) ( e1, e2 ) then
        True

    else
        case ( e1, e2 ) of
            ( Expr.One, Expr.One ) ->
                False

            ( Expr.Zero, Expr.Zero ) ->
                False

            ( Ident _, Ident _ ) ->
                False

            ( Neg a, Neg b ) ->
                wasAppliedRepl ( p1, p2 ) ( a, b )

            ( And a1 b1, And a2 b2 ) ->
                wasAppliedRepl ( p1, p2 ) ( a1, a2 ) |> Basics.xor (wasAppliedRepl ( p1, p2 ) ( b1, b2 ))

            ( Or a1 b1, Or a2 b2 ) ->
                wasAppliedRepl ( p1, p2 ) ( a1, a2 ) |> Basics.xor (wasAppliedRepl ( p1, p2 ) ( b1, b2 ))

            ( Implies a1 b1, Implies a2 b2 ) ->
                wasAppliedRepl ( p1, p2 ) ( a1, a2 ) |> Basics.xor (wasAppliedRepl ( p1, p2 ) ( b1, b2 ))

            ( Iff a1 b1, Iff a2 b2 ) ->
                wasAppliedRepl ( p1, p2 ) ( a1, a2 ) |> Basics.xor (wasAppliedRepl ( p1, p2 ) ( b1, b2 ))

            ( _, _ ) ->
                False


wasLiterallyAppliedReplHere : ( Expr, Expr ) -> ( Expr, Expr ) -> Bool
wasLiterallyAppliedReplHere ( p1, p2 ) ( e1, e2 ) =
    case Maybe.andThen (tryMatch e2 p2) (tryMatch e1 p1 Dict.empty) of
        Just v ->
            Just e2 == replaceAll p2 v

        -- Just e2 == replaceAll p2 v
        Nothing ->
            False


{-| Given the theory, check if the given expression can be inferred
from the hypothesis (just check if any of those is equal to this)
-}
tryFromHypothesis : List Expr -> Maybe Expr -> Expr -> Bool
tryFromHypothesis lst assummed ex =
    List.any ((==) ex) lst || assummed == Just ex


{-| Given the list of steps, check if the given expression is
result of monotony
-}
tryFromMonotony : List { assumed : Maybe Expr, what : Expr, num : Int } -> Expr -> Maybe Int
tryFromMonotony lst ex =
    List.filter (\e -> e.assumed == Nothing && e.what == ex) lst
        |> List.head
        |> Maybe.map (\e -> e.num)


{-| Try to infer from replacement
-}
tryFromReplacement : List { number : Int, ex : Expr } -> Expr -> Maybe { ref : Int, name : String }
tryFromReplacement exs new =
    List.filterMap (tryFromReplacementFilter new) exs |> List.head


tryFromReplacementFilter : Expr -> { number : Int, ex : Expr } -> Maybe { name : String, ref : Int }
tryFromReplacementFilter res { number, ex } =
    case
        List.filterMap
            (tryFromReplacementFilterCheck res ex)
            allEquivalences
    of
        equiv :: _ ->
            Just { name = equiv, ref = number }

        [] ->
            Nothing


tryFromReplacementFilterCheck : Expr -> Expr -> ( String, Expr, Expr ) -> Maybe String
tryFromReplacementFilterCheck res ex ( name, p1, p2 ) =
    if wasApplied (Replacement p1 p2) ex res then
        Just name

    else
        Nothing


tryFromImplication : List { number : Int, ex : Expr } -> Expr -> Maybe { name : String, ref : Int }
tryFromImplication lst res =
    case List.filterMap (tryFromImplicationFilter res) (List.map (\e -> ( e.number, e.ex )) lst) of
        { implName, ref } :: _ ->
            Just { name = implName, ref = ref }

        [] ->
            Nothing


tryFromImplicationFilter : Expr -> ( Int, Expr ) -> Maybe { implName : String, ref : Int }
tryFromImplicationFilter res ( r, other ) =
    case List.filterMap (tryFromImplicationCheck res other) allImplications of
        i :: _ ->
            Just { implName = i, ref = r }

        [] ->
            Nothing


tryFromImplicationCheck : Expr -> Expr -> ( String, Expr, Expr ) -> Maybe String
tryFromImplicationCheck res other ( name, p1, p2 ) =
    if wasApplied (LogicalImplication p1 p2) other res then
        Just name

    else
        Nothing


type InferenceRefs
    = One Int
    | Two Int Int


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
            tryMatch assum rule.assum Dict.empty
                |> Maybe.andThen (tryMatch oex.what rule.what)
                |> Maybe.andThen (tryMatch ex rule.thesis)
                |> Maybe.andThen (replaceAll rule.thesis)
                |> Maybe.andThen
                    (\e ->
                        if e == ex then
                            Just { name = rule.name, refs = One oex.number }

                        else
                            Nothing
                    )

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
    tryMatch oex.what pat.what Dict.empty
        |> Maybe.andThen (tryMatch ex pat.thesis)
        |> Maybe.andThen (replaceAll pat.thesis)
        |> Maybe.andThen
            (\e ->
                if e == ex then
                    Just { name = pat.name, refs = One oex.number }

                else
                    Nothing
            )


tryFromInferenceFilterMatch2 :
    Expr
    -> ( { number : Int, what : Expr }, ( { number : Int, what : Expr }, { name : String, what1 : Expr, what2 : Expr, thesis : Expr } ) )
    -> Maybe { refs : InferenceRefs, name : String }
tryFromInferenceFilterMatch2 ex ( e1, ( e2, pat ) ) =
    tryMatch e1.what pat.what1 Dict.empty
        |> Maybe.andThen (tryMatch e2.what pat.what2)
        |> Maybe.andThen (tryMatch ex pat.thesis)
        |> Maybe.andThen (replaceAll pat.thesis)
        |> Maybe.andThen
            (\e ->
                if e == ex then
                    Just { name = pat.name, refs = Two e1.number e2.number }

                else
                    Nothing
            )
