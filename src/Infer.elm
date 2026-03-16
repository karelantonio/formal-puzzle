module Infer exposing (Transformation(..), tryFromHypothesis, tryFromImplication, tryFromMonotony, tryFromReplacement, wasApplied)

{-| Check if this some expressions can be inferred from others
-}

import AllRules exposing (..)
import Debug exposing (todo)
import Dict
import Expr exposing (..)
import Match exposing (..)
import Tuple exposing (first)
import Utils exposing (isJust)


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
tryFromReplacement : List { number : Int, ex : Expr } -> Expr -> Maybe { ref : Int, which : Int }
tryFromReplacement exs new =
    case List.filterMap (tryFromReplacementFilter new) exs of
        hd :: _ ->
            Just hd

        [] ->
            Nothing


tryFromReplacementFilter : Expr -> { number : Int, ex : Expr } -> Maybe { which : Int, ref : Int }
tryFromReplacementFilter res { number, ex } =
    case
        List.filterMap (tryFromReplacementFilterCheck res ex) (List.indexedMap Tuple.pair allEquivalences)
    of
        equiv :: _ ->
            Just { which = equiv, ref = number }

        [] ->
            Nothing


tryFromReplacementFilterCheck : Expr -> Expr -> ( Int, ( Expr, Expr ) ) -> Maybe Int
tryFromReplacementFilterCheck res ex ( i, ( p1, p2 ) ) =
    if wasApplied (Replacement p1 p2) ex res then
        Just i

    else
        Nothing


tryFromImplication : List { number : Int, ex : Expr } -> Expr -> Maybe { what : Int, ref : Int }
tryFromImplication lst res =
    case List.filterMap (tryFromImplicationFilter res) (List.map (\e -> ( e.number, e.ex )) lst) of
        { impl, ref } :: _ ->
            Just { what = impl, ref = ref }

        [] ->
            Nothing


tryFromImplicationFilter : Expr -> ( Int, Expr ) -> Maybe { impl : Int, ref : Int }
tryFromImplicationFilter res ( r, other ) =
    case List.filterMap (tryFromImplicationCheck res other) (List.indexedMap Tuple.pair allImplications) of
        i :: _ ->
            Just { impl = i, ref = r }

        [] ->
            Nothing


tryFromImplicationCheck : Expr -> Expr -> ( Int, ( Expr, Expr ) ) -> Maybe Int
tryFromImplicationCheck res other ( i, ( p1, p2 ) ) =
    if wasApplied (LogicalImplication p1 p2) other res then
        Just i

    else
        Nothing
