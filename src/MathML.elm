module MathML exposing (exprToMathML, exprToMathMLTag)

{-| Convert expressions to mathml tags
-}

import Expr exposing (..)
import Html exposing (Html, node, text)


exprToMathML : Expr -> Html msg
exprToMathML ex =
    node "math" [] [ node "mrow" [] (exprToMathMLTag ex) ]


exprToMathMLTag : Expr -> List (Html msg)
exprToMathMLTag ex =
    case ex of
        Ident name ->
            List.map (\c -> node "mi" [] [ text (String.fromChar c) ]) (String.toList name)

        One ->
            [ node "mn" [] [ text "1" ] ]

        Zero ->
            [ node "mn" [] [ text "0" ] ]

        Neg sub ->
            node "mo" [] [ text "¬" ] :: exprToMathMLTag sub

        And a b ->
            exprToMathMLTagPar a ++ (node "mo" [] [ text "∧" ] :: exprToMathMLTagPar b)

        Or a b ->
            exprToMathMLTagPar a ++ (node "mo" [] [ text "∨" ] :: exprToMathMLTagPar b)

        Implies a b ->
            exprToMathMLTagPar a ++ (node "mo" [] [ text "⟹" ] :: exprToMathMLTagPar b)

        Iff a b ->
            exprToMathMLTagPar a ++ (node "mo" [] [ text "⟺" ] :: exprToMathMLTagPar b)


exprToMathMLTagPar : Expr -> List (Html msg)
exprToMathMLTagPar ex =
    if isSimple ex then
        exprToMathMLTag ex

    else
        node "mo" [] [ text "(" ] :: exprToMathMLTag ex ++ [ node "mo" [] [ text ")" ] ]
