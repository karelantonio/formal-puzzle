module Expr.Utils exposing (isSimple, parToString, toString)

import Expr.Types exposing (..)


{-| Check if an expression is simple
-}
isSimple : Expr -> Bool
isSimple expr =
    case expr of
        One ->
            True

        Zero ->
            True

        Ident _ ->
            True

        Neg _ ->
            True

        And _ _ ->
            False

        Or _ _ ->
            False

        Implies _ _ ->
            False

        Iff _ _ ->
            False


{-| Convert an expression to string
-}
toString : Expr -> String
toString expr =
    case expr of
        One ->
            "1"

        Zero ->
            "0"

        Ident id ->
            id

        Neg sub ->
            "-" ++ parToString sub

        And left right ->
            parToString left ++ "&" ++ parToString right

        Or left right ->
            parToString left ++ "|" ++ parToString right

        Implies left right ->
            parToString left ++ "->" ++ parToString right

        Iff left right ->
            parToString left ++ "<->" ++ parToString right


{-| Add parenthesis around if is not simple
-}
parToString : Expr -> String
parToString expr =
    if isSimple expr then
        toString expr

    else
        "(" ++ toString expr ++ ")"
