module Expr.Types exposing (Expr(..), InferenceRefs)

{-| An expression
-}


type Expr
    = One
    | Zero
    | Ident String
    | Neg Expr
    | And Expr Expr
    | Or Expr Expr
    | Implies Expr Expr
    | Iff Expr Expr


type InferenceRefs
    = OneRef Int
    | TwoRefs Int Int
