module Match exposing (tryMatch)

import Dict exposing (Dict)
import Expr exposing (..)


{-| Matching patterns against expressions, the first argument is the
expression and the second is the pattern
-}
tryMatch : Expr -> Expr -> Dict String Expr -> Maybe (Dict String Expr)
tryMatch ex pat dik =
    case ( ex, pat ) of
        ( Expr.One, Expr.One ) ->
            Just Dict.empty

        ( Expr.Zero, Expr.Zero ) ->
            Just Dict.empty

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
