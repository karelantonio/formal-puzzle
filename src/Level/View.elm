module Level.View exposing (view)

import Browser exposing (..)
import Expr.Types exposing (Expr(..))
import Html exposing (Html, div, form, h3, input, node, p, table, tbody, td, text, tr)
import Html.Attributes exposing (class, colspan, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Infer.Types exposing (Transformation(..))
import Level.Types exposing (..)
import MathML exposing (exprToMathML)



-- The view


view : Model -> Document Msg
view m =
    case m of
        Ex ex ->
            { title = "Ejercicio"
            , body =
                [ div [ class "exercise-ui" ]
                    [ div [ class "exercise-content", id "exercise-content" ]
                        [ theory ex.descr ex.goal
                        , theSteps ex.steps
                        , parseError ex.error_msg
                        ]
                    , bottomBar ex.ded_text
                    ]
                ]
            }


bottomBar : String -> Html Msg
bottomBar dedtext =
    div
        [ class "exercise-bottom-bar" ]
        [ form [ onSubmit AddPressed ]
            [ input
                [ placeholder "Escribe la deducción aquí"
                , value dedtext
                , onInput (\t -> DeductionTextChanged t)
                , class "exercise-bottom-bar-input"
                , id "exercise-bottom-bar-input"
                , type_ "text"
                ]
                []
            , div [ class "exercise-bottom-bar-buttons" ]
                [ input
                    [ type_ "button"
                    , class "exercise-bottom-bar-btn"
                    , onClick (InsertPressed "∀")
                    , value "∀"
                    ]
                    []
                , input
                    [ type_ "button"
                    , class "exercise-bottom-bar-btn"
                    , onClick (InsertPressed "∃")
                    , value "∃"
                    ]
                    []
                , input
                    [ type_ "button"
                    , class "exercise-bottom-bar-btn"
                    , onClick (InsertPressed "-")
                    , value "¬"
                    ]
                    []
                , input
                    [ type_ "button"
                    , class "exercise-bottom-bar-btn"
                    , onClick (InsertPressed "->")
                    , value "⟹"
                    ]
                    []
                , input
                    [ type_ "button"
                    , class "exercise-bottom-bar-btn"
                    , onClick (InsertPressed "<->")
                    , value "⟺"
                    ]
                    []
                , input
                    [ type_ "button"
                    , class "exercise-bottom-bar-btn"
                    , onClick (InsertPressed "&")
                    , value "∧"
                    ]
                    []
                , input
                    [ type_ "button"
                    , class "exercise-bottom-bar-btn"
                    , onClick (InsertPressed "|")
                    , value "∨"
                    ]
                    []
                , div [ id "separator" ] []
                , input
                    [ type_ "button"
                    , class "exercise-bottom-bar-btn"
                    , onClick AddPressed
                    , value "+"
                    ]
                    []
                , input
                    [ type_ "button"
                    , class "exercise-bottom-bar-btn"
                    , onClick TheoryPressed
                    , value "T"
                    ]
                    []
                ]
            ]
        ]


parseError : Maybe String -> Html Msg
parseError err =
    case err of
        Just msg ->
            div [ class "exercise-parse-err" ] [ div [] [ text msg ] ]

        Nothing ->
            div [ class "exercise-parse-err" ] []


theSteps : List Step -> Html Msg
theSteps steps =
    div [ class "exercise-ui-steps" ]
        [ table [] [ tbody [] (steps2html steps |> List.reverse) ] ]


steps2html : List Step -> List (Html Msg)
steps2html steps =
    case steps of
        [] ->
            []

        -- Just next to the assuming step
        (Deduction ded) :: (Assume a) :: tl ->
            step2htmlDeduction (Just a) ded :: steps2html tl

        (Assume a) :: tl ->
            step2htmlAssume a :: steps2html tl

        (Deduction ded) :: tl ->
            step2htmlDeduction Nothing ded :: steps2html tl


step2htmlAssume : Maybe Expr -> Html Msg
step2htmlAssume maex =
    tr [ class "exercise-step-deduction" ]
        [ td [] [ step2htmlAssumeCore maex ]
        , td [] [ deductionSymbol ]
        , td [ colspan 3, class "exercise-step-deduction-expression exercise-step-deduction-nothing" ] [ text "(Nada todavía, haz tus deducciones)" ]
        ]


step2htmlAssumeCore : Maybe Expr -> Html Msg
step2htmlAssumeCore maex =
    case maex of
        Just what ->
            div [ class "clickable", onClick (ExprPressed what) ]
                [ text "T,"
                , exprToMathML what
                ]

        Nothing ->
            text "T"


step2htmlDeduction : Maybe (Maybe Expr) -> { assumed : Maybe Expr, num : Int, what : Expr, reason : Reason } -> Html Msg
step2htmlDeduction firstAssuming ded =
    tr [ class "exercise-step-deduction" ]
        [ case firstAssuming of
            Just v ->
                td [] [ step2htmlAssumeCore v ]

            Nothing ->
                td [] []
        , td [ class "exercise-step-deduction-symbol" ]
            [ deductionSymbol ]
        , td [ onClick (ExprPressed ded.what), class "exercise-step-deduction-expression clickable" ]
            [ exprToMathML ded.what ]
        , td [ class "exercise-step-deduction-reason" ]
            [ text (reasonToString ded.reason) ]
        , td [ class "exercise-step-deduction-stepnum" ]
            [ text ("(" ++ String.fromInt ded.num ++ ")") ]
        ]


reasonToString : Reason -> String
reasonToString reason =
    case reason of
        Hypotesis ->
            "Hip"

        Monotony ref ->
            "Monot:" ++ String.fromInt ref

        Equivalence args ->
            args.name ++ ":" ++ String.fromInt args.ref

        Implication args ->
            args.name ++ ":" ++ String.fromInt args.ref

        InferenceRule1 args ->
            args.name ++ ":" ++ String.fromInt args.ref1

        InferenceRule2 args ->
            args.name ++ ":" ++ String.fromInt args.ref1 ++ "," ++ String.fromInt args.ref2


deductionSymbol : Html Msg
deductionSymbol =
    node "math" [] [ node "mrow" [] [ node "mo" [] [ text "⊢" ] ] ]


theory : List DescrItem -> Expr -> Html Msg
theory descr goal =
    div [ class "theory" ]
        (h3 [] [ text "Teoría:" ]
            -- :: p [ id "theory-description" ] [ text descr ]
            :: List.map theoryItem descr
            ++ [ h3 [] [ text "Objetivo:" ]
               , theoryItem (Theory goal)
               ]
        )


theoryItem : DescrItem -> Html Msg
theoryItem itm =
    case itm of
        Text txt ->
            div [ class "theory-description" ] [ text txt ]

        Theory ex ->
            div [ class "theory-item clickable", onClick (ExprPressed ex) ] [ MathML.exprToMathML ex ]
