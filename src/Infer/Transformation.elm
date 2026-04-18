module Infer.Transformation exposing (..)

{-| Check if the second expression can be inferred from the first one using the given transformation
-}

import AllRules exposing (allEquivalences, allImplications)
import Dict
import Expr.Types exposing (Expr(..))
import Infer.Types exposing (Transformation(..))
import Match exposing (replaceAll, tryMatch)


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
            ( One, One ) ->
                False

            ( Zero, Zero ) ->
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
