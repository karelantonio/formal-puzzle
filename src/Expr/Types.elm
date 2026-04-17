module Expr.Types exposing (Expr(..))

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
