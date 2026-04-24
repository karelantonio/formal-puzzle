module MathML exposing (exprToMathML, exprToMathMLTag)

{-| Convert expressions to mathml tags
-}

import Expr.Types exposing (Expr(..), FunTree(..), isSimple)
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
            node "mo" [] [ text "¬" ] :: exprToMathMLTagPar sub

        And a b ->
            exprToMathMLTagPar a ++ (node "mo" [] [ text "∧" ] :: exprToMathMLTagPar b)

        Or a b ->
            exprToMathMLTagPar a ++ (node "mo" [] [ text "∨" ] :: exprToMathMLTagPar b)

        Implies a b ->
            exprToMathMLTagPar a ++ (node "mo" [] [ text "⟹" ] :: exprToMathMLTagPar b)

        Iff a b ->
            exprToMathMLTagPar a ++ (node "mo" [] [ text "⟺" ] :: exprToMathMLTagPar b)

        Predicate name args ->
            -- Should probably do something better here in a future
            -- WTF am I doing with my life???
            case ( name, args ) of
                ( "=", [ l, r ] ) ->
                    funTreeToMathMLTag l ++ node "mo" [] [ text "=" ] :: funTreeToMathMLTag r

                _ ->
                    node "mi" [] [ text name ]
                        :: node "mo" [] [ text "(" ]
                        :: (joinWith [ node "mo" [] [ text "," ] ] (List.map funTreeToMathMLTag args) |> List.concat)
                        ++ [ node "mo" [] [ text ")" ] ]

        Forall name sub ->
            node "mo" [] [ text "∀" ]
                :: node "mo" [] [ text "(" ]
                :: node "mi" [] [ text name ]
                :: node "mo" [] [ text ")" ]
                :: exprToMathMLTagPar sub

        Exists name sub ->
            node "mo" [] [ text "∃" ]
                :: node "mo" [] [ text "(" ]
                :: node "mi" [] [ text name ]
                :: node "mo" [] [ text ")" ]
                :: exprToMathMLTagPar sub


exprToMathMLTagPar : Expr -> List (Html msg)
exprToMathMLTagPar ex =
    if isSimple ex then
        exprToMathMLTag ex

    else
        node "mo" [] [ text "(" ] :: exprToMathMLTag ex ++ [ node "mo" [] [ text ")" ] ]


funTreeToMathMLTag : FunTree -> List (Html msg)
funTreeToMathMLTag tr =
    case tr of
        Atom name ->
            [ node "mi" [] [ text name ] ]

        Apply name args ->
            node "mi" [] [ text name ]
                :: node "mo" [] [ text "(" ]
                :: ((joinWith [ node "mo" [] [ text "," ] ] <| List.map funTreeToMathMLTag args) |> List.concat)
                ++ [ node "mo" [] [ text ")" ] ]


joinWith : a -> List a -> List a
joinWith val lst =
    case lst of
        hd :: tl ->
            hd :: List.concatMap (\e -> [ val, e ]) tl

        [] ->
            []
