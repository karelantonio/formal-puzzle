module Expr.Types exposing (Expr(..), FunTree(..), InferenceRefs, extractAllKnownPropositions, isSimple, parToString, toString, toStringFunTree)

{-| An expression
-}

import Set exposing (Set)


type FunTree
    = Variable String
    | Value String
    | Apply String (List FunTree)


type Expr
    = One
    | Zero
    | Ident String
    | Neg Expr
    | And Expr Expr
    | Or Expr Expr
    | Implies Expr Expr
    | Iff Expr Expr
    | Predicate String (List FunTree)
    | Forall String Expr
    | Exists String Expr


type InferenceRefs
    = OneRef Int
    | TwoRefs Int Int


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

        Predicate _ _ ->
            True

        Forall _ _ ->
            True

        Exists _ _ ->
            True


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

        Predicate name args ->
            name ++ "(" ++ String.join "," (List.map toStringFunTree args) ++ ")"

        Forall name sub ->
            "∀(" ++ name ++ ")" ++ parToString sub

        Exists name sub ->
            "∃(" ++ name ++ ")" ++ parToString sub


toStringFunTree : FunTree -> String
toStringFunTree ftr =
    case ftr of
        Variable name ->
            name

        Value name ->
            name

        Apply name args ->
            name ++ "(" ++ String.join "," (List.map toStringFunTree args) ++ ")"


{-| Add parenthesis around if is not simple
-}
parToString : Expr -> String
parToString expr =
    if isSimple expr then
        toString expr

    else
        "(" ++ toString expr ++ ")"



-- Extract known propositions


extractAllKnownPropositions : List Expr -> Set String
extractAllKnownPropositions lst =
    List.foldl extractKnownPropositions Set.empty lst


extractKnownPropositions : Expr -> Set String -> Set String
extractKnownPropositions ex st =
    case ex of
        One ->
            st

        Zero ->
            st

        Ident x ->
            Set.insert x st

        Neg e ->
            extractKnownPropositions e st

        And l r ->
            extractKnownPropositions l st |> extractKnownPropositions r

        Or l r ->
            extractKnownPropositions l st |> extractKnownPropositions r

        Implies l r ->
            extractKnownPropositions l st |> extractKnownPropositions r

        Iff l r ->
            extractKnownPropositions l st |> extractKnownPropositions r

        Predicate _ _ ->
            st

        Forall _ sub ->
            extractKnownPropositions sub st

        Exists _ sub ->
            extractKnownPropositions sub st


{-| Get the arity of a function (number of args) or 0 if is a constant
-}
arity : FunTree -> Int
arity tr =
    case tr of
        Value _ ->
            0

        Variable _ ->
            0

        Apply _ arg ->
            List.length arg
