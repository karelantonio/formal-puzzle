module Infer exposing (Transformation(..), tryFromHypothesis, tryFromMonotony, tryFromReplacement, wasApplied)

{-| Check if this some expressions can be inferred from others
-}

import AllRules exposing (..)
import Dict
import Expr exposing (..)
import Match exposing (..)
import Tuple exposing (first)


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
tryFromReplacement : List Expr -> Expr -> Maybe { ref : Int, which : Int }
tryFromReplacement exs new =
    List.foldl (tryFromReplacementFold new) ( Nothing, 0 ) exs |> first


tryFromReplacementFold : Expr -> Expr -> ( Maybe { ref : Int, which : Int }, Int ) -> ( Maybe { ref : Int, which : Int }, Int )
tryFromReplacementFold e2 e1 ( prev, idx ) =
    case prev of
        Just i ->
            ( Just i, idx + 1 )

        Nothing ->
            let
                res =
                    List.foldl (tryFromReplacementWithOtherFold ( e1, e2 )) ( Nothing, 0 ) AllRules.allEquivalences
            in
            case res of
                ( Just a, _ ) ->
                    ( Just { which = a, ref = idx }, idx + 1 )

                ( Nothing, _ ) ->
                    ( Nothing, idx + 1 )


tryFromReplacementWithOtherFold : ( Expr, Expr ) -> ( Expr, Expr ) -> ( Maybe Int, Int ) -> ( Maybe Int, Int )
tryFromReplacementWithOtherFold ( e1, e2 ) ( p1, p2 ) ( prev, idx ) =
    case prev of
        Just a ->
            ( Just a, idx + 1 )

        Nothing ->
            if wasApplied (Replacement p1 p2) e1 e2 then
                ( Just idx, idx + 1 )

            else
                ( Nothing, idx + 1 )
