module Infer exposing (Transformation(..), tryFromHypothesis, tryFromMonotony, wasApplied)

{-| Check if this some expressions can be inferred from others
-}

import Dict
import Expr exposing (..)
import Match exposing (..)


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

            ( Ident a, Ident b ) ->
                a == b

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
