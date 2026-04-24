module Expr.Types exposing
    ( Domain
    , Expr(..)
    , FunTree(..)
    , InferenceRefs
    , Loc(..)
    , ParseError
    , arity
    , emptyDomain
    , extractDomainFromTheory
    , isSimple
    , isVariable
    , parToString
    , toString
    , toStringFunTree
    )

{-| An expression
-}

import Dict exposing (Dict)
import Set exposing (Set)


type FunTree
    = Atom String
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


type Loc
    = Pos Int
    | End


{-| The location and cause of the error
-}
type alias ParseError =
    { location : Loc, msg : String }


type alias Domain =
    { domain : Set String, propositions : Set String, predicates : Dict String Int, functions : Dict String Int }


{-| Empty domain
-}
emptyDomain : Domain
emptyDomain =
    { domain = Set.empty, propositions = Set.empty, predicates = Dict.empty, functions = Dict.empty }


{-| Extract the domain, it is safe to assume that the happy path, as this is only provided by the dev
-}
extractDomainFromTheory : List Expr -> Domain
extractDomainFromTheory =
    List.foldl extractDomainFromExpr emptyDomain


extractDomainFromExpr : Expr -> Domain -> Domain
extractDomainFromExpr ex dom =
    case ex of
        One ->
            dom

        Zero ->
            dom

        Ident name ->
            { dom | propositions = Set.insert name dom.propositions }

        Neg e ->
            extractDomainFromExpr e dom

        And l r ->
            extractDomainFromExpr l dom |> extractDomainFromExpr r

        Or l r ->
            extractDomainFromExpr l dom |> extractDomainFromExpr r

        Implies l r ->
            extractDomainFromExpr l dom |> extractDomainFromExpr r

        Iff l r ->
            extractDomainFromExpr l dom |> extractDomainFromExpr r

        Predicate name args ->
            List.foldl extractDomainFromFunTree { dom | predicates = Dict.insert name (List.length args) dom.predicates } args

        Forall var sub ->
            let
                subdom =
                    extractDomainFromExpr sub dom
            in
            { subdom | domain = Set.remove var subdom.domain }

        Exists var sub ->
            let
                subdom =
                    extractDomainFromExpr sub dom
            in
            { subdom | domain = Set.remove var subdom.domain }


extractDomainFromFunTree : FunTree -> Domain -> Domain
extractDomainFromFunTree ftr dom =
    case ftr of
        Atom name ->
            { dom | domain = Set.insert name dom.domain }

        Apply name args ->
            List.foldl extractDomainFromFunTree { dom | functions = Dict.insert name (List.length args) dom.functions } args


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
        Atom name ->
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


{-| Get the arity of a function (number of args) or 0 if is a constant
-}
arity : FunTree -> Int
arity tr =
    case tr of
        Atom _ ->
            0

        Apply _ arg ->
            List.length arg


{-| Check if a given name belongs to a variable or a value
-}
isVariable : String -> Bool
isVariable name =
    String.toList name |> List.head |> Maybe.map Char.isLower |> Maybe.withDefault False
