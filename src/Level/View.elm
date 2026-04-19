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
                        [ theory ex.descr ex.theory ex.goal
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
                , type_ "text"
                ]
                []
            , div [ class "exercise-bottom-bar-buttons" ]
                [ div [ id "separator" ] []
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
        [ table [] [ tbody [] (List.map step2html steps |> List.reverse) ] ]


step2html : Step -> Html Msg
step2html step =
    case step of
        Assume a ->
            step2htmlAssume a

        Deduction ded ->
            step2htmlDeduction ded


step2htmlAssume : Maybe Expr -> Html Msg
step2htmlAssume maex =
    case maex of
        Just what ->
            tr [ class "exercise-step-deduction" ]
                [ td []
                    [ div [ class "clickable", onClick (ExprPressed what) ]
                        [ text "T,"
                        , exprToMathML what
                        ]
                    ]
                , td [] [ deductionSymbol ]
                , td [ colspan 3, class "exercise-step-deduction-expression exercise-step-deduction-nothing" ] [ text "(Nada todavía, haz tus deducciones)" ]
                ]

        Nothing ->
            tr [ class "exercise-step-deduction" ]
                [ td []
                    [ text "T"
                    ]
                , td [] [ deductionSymbol ]
                , td [ colspan 3, class "exercise-step-deduction-expression exercise-step-deduction-nothing" ] [ text "(Nada todavía, haz tus deducciones)" ]
                ]


step2htmlDeduction : { assumed : Maybe Expr, num : Int, what : Expr, reason : Reason } -> Html Msg
step2htmlDeduction ded =
    tr [ class "exercise-step-deduction" ]
        [ td [] []
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


theory : String -> List Expr -> Expr -> Html Msg
theory descr lst goal =
    div [ class "theory" ]
        (h3 [] [ text "Teoría:" ]
            :: p [ id "theory-description" ] [ text descr ]
            :: List.map theoryItem lst
            ++ [ h3 [] [ text "Objetivo:" ]
               , theoryItem goal
               ]
        )


theoryItem : Expr -> Html Msg
theoryItem ex =
    div [ class "theory-item clickable", onClick (ExprPressed ex) ] [ MathML.exprToMathML ex ]
