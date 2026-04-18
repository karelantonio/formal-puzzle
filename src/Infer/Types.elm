module Infer.Types exposing (InferenceRefs(..), Transformation(..))

import Expr.Types exposing (Expr)


type InferenceRefs
    = OneRef Int
    | TwoRefs Int Int


{-| A transformation
-}
type Transformation
    = Replacement Expr Expr
    | LogicalImplication Expr Expr
