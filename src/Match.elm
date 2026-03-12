module Match exposing (replaceAll, tryMatch)

import Debug exposing (todo)
import Dict exposing (Dict)
import Expr exposing (..)


{-| Matching patterns against expressions, the first argument is the
expression and the second is the pattern
-}
tryMatch : Expr -> Expr -> Dict String Expr -> Maybe (Dict String Expr)
tryMatch ex pat dik =
    case ( ex, pat ) of
        ( Expr.One, Expr.One ) ->
            Just dik

        ( Expr.Zero, Expr.Zero ) ->
            Just dik

        ( Expr.Neg a, Expr.Neg b ) ->
            tryMatch a b dik

        ( Expr.And l1 r1, Expr.And l2 r2 ) ->
            tryMatch l1 l2 dik |> Maybe.andThen (\d -> tryMatch r1 r2 d)

        ( Expr.Or l1 r1, Expr.Or l2 r2 ) ->
            tryMatch l1 l2 dik |> Maybe.andThen (\d -> tryMatch r1 r2 d)

        ( Expr.Implies l1 r1, Expr.Implies l2 r2 ) ->
            tryMatch l1 l2 dik |> Maybe.andThen (\d -> tryMatch r1 r2 d)

        ( Expr.Iff l1 r1, Expr.Iff l2 r2 ) ->
            tryMatch l1 l2 dik |> Maybe.andThen (\d -> tryMatch r1 r2 d)

        ( ex2, Expr.Ident name ) ->
            case Dict.get name dik of
                Just v ->
                    if v == ex2 then
                        Just dik

                    else
                        Nothing

                Nothing ->
                    Just (Dict.insert name ex2 dik)

        _ ->
            Nothing


{-| Replace the identifiers with the given expressions
-}
replaceAll : Expr -> Dict String Expr -> Maybe Expr
replaceAll ex dik =
    case ex of
        Expr.Ident name ->
            Dict.get name dik

        One ->
            Just One

        Zero ->
            Just Zero

        Neg sub ->
            Maybe.map Neg (replaceAll sub dik)

        And l r ->
            Maybe.map2 And (replaceAll l dik) (replaceAll r dik)

        Or l r ->
            Maybe.map2 Or (replaceAll l dik) (replaceAll r dik)

        Implies l r ->
            Maybe.map2 Implies (replaceAll l dik) (replaceAll r dik)

        Iff l r ->
            Maybe.map2 Iff (replaceAll l dik) (replaceAll r dik)
