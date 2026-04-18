module Match exposing (replaceAll, tryMatch)

import Dict exposing (Dict)
import Expr.Types exposing (Expr(..))


{-| Matching patterns against expressions, the first argument is the
expression and the second is the pattern
-}
tryMatch : Expr -> Expr -> Dict String Expr -> Maybe (Dict String Expr)
tryMatch ex pat dik =
    case ( ex, pat ) of
        ( One, One ) ->
            Just dik

        ( Zero, Zero ) ->
            Just dik

        ( Neg a, Neg b ) ->
            tryMatch a b dik

        ( And l1 r1, And l2 r2 ) ->
            tryMatch l1 l2 dik |> Maybe.andThen (\d -> tryMatch r1 r2 d)

        ( Or l1 r1, Or l2 r2 ) ->
            tryMatch l1 l2 dik |> Maybe.andThen (\d -> tryMatch r1 r2 d)

        ( Implies l1 r1, Implies l2 r2 ) ->
            tryMatch l1 l2 dik |> Maybe.andThen (\d -> tryMatch r1 r2 d)

        ( Iff l1 r1, Iff l2 r2 ) ->
            tryMatch l1 l2 dik |> Maybe.andThen (\d -> tryMatch r1 r2 d)

        ( ex2, Ident name ) ->
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
        Ident name ->
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

        Predicate _ _ ->
            Just ex

        Forall _ sub ->
            replaceAll sub dik

        Exists _ sub ->
            replaceAll sub dik
