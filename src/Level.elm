module Level exposing (..)

import Browser exposing (..)
import Expr exposing (..)
import Html exposing (Html, div, form, h3, input, node, p, table, tbody, td, text, tr)
import Html.Attributes exposing (class, colspan, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Infer exposing (Transformation(..))
import LevelTys exposing (..)
import MathML exposing (exprToMathML)
import TeaCommon exposing (..)
import Utils



-- The message
-- Update the view


update : LevelMsg -> LevelModel -> ( Model, Cmd Msg )
update msg model =
    case model of
        Ex ex ->
            updateEx msg ex


updateEx :
    LevelMsg
    -> { descr : String, goal : Expr, ded_text : String, error_msg : Maybe String, theory : List Expr, steps : List Step }
    -> ( Model, Cmd Msg )
updateEx msg ex =
    case msg of
        DeductionTextChanged txt ->
            ( Ex { ex | ded_text = txt, error_msg = Nothing } |> LevelModelV, Cmd.none )

        AddPressed ->
            case parse ex.ded_text of
                Ok parsed_ex ->
                    case tryToInferDed { theory = ex.theory, steps = ex.steps } parsed_ex of
                        Just ded ->
                            ( Ex
                                { ex
                                    | ded_text = ""
                                    , steps = ded :: ex.steps
                                }
                                |> LevelModelV
                            , Utils.scrollToBottom "exercise-content"
                            )

                        Nothing ->
                            ( Ex
                                { ex
                                    | error_msg = Just "Al parecer no hay regla de inferencia, equivalencia lógica, implicación lógica, monotonía o hipótesis que justifique eso"
                                }
                                |> LevelModelV
                            , Utils.scrollToBottom "exercise-ui"
                            )

                Err err ->
                    ( Ex
                        { ex
                            | error_msg = err.msg ++ " (Posición: " ++ String.fromInt (err.location + 1) ++ ")" |> Just
                        }
                        |> LevelModelV
                    , Utils.scrollToBottom "exercise-ui"
                    )

        TheoryPressed ->
            if String.isEmpty ex.ded_text then
                ( Ex { ex | steps = addAssumeToSteps Nothing ex.steps } |> LevelModelV, Cmd.none )

            else
                case parse ex.ded_text of
                    Ok parsed_ex ->
                        ( Ex { ex | steps = addAssumeToSteps (Just parsed_ex) ex.steps } |> LevelModelV, Cmd.none )

                    Err err ->
                        ( Ex { ex | error_msg = err.msg ++ " (Posición: " ++ String.fromInt err.location ++ ")" |> Just } |> LevelModelV, Cmd.none )

        ExprPressed what ->
            ( Ex { ex | ded_text = toString what, error_msg = Nothing } |> LevelModelV, Cmd.none )


addAssumeToSteps : Maybe Expr -> List Step -> List Step
addAssumeToSteps ex steps =
    case steps of
        (Assume _) :: rem ->
            if currAssumed rem == ex && List.isEmpty rem |> not then
                rem

            else
                Assume ex :: rem

        _ ->
            if currAssumed steps == ex then
                steps

            else
                Assume ex :: steps


tryToInferDed : { theory : List Expr, steps : List Step } -> Expr -> Maybe Step
tryToInferDed mod ex =
    Maybe.map
        (\reason ->
            Deduction
                { assumed = currAssumed mod.steps
                , num = nextNumber mod.steps
                , what = ex
                , reason = reason
                }
        )
        (tryToInfer mod ex)


tryToInfer : { theory : List Expr, steps : List Step } -> Expr -> Maybe Reason
tryToInfer model ex =
    if Infer.tryFromHypothesis model.theory (currAssumed model.steps) ex then
        Just Hypotesis

    else
        case tryToInferMonot model.steps ex of
            Just mon ->
                Just mon

            Nothing ->
                let
                    stps =
                        extractInSameTheory model.steps
                in
                case tryToInferRepl stps ex of
                    Just val ->
                        Just val

                    Nothing ->
                        case tryToInferImpl stps ex of
                            Just val ->
                                Just val

                            Nothing ->
                                tryToInferRule model.steps ex


tryToInferMonot : List Step -> Expr -> Maybe Reason
tryToInferMonot lst ex =
    Maybe.andThen
        (\_ ->
            Maybe.map Monotony
                (Infer.tryFromMonotony
                    (List.filterMap
                        tryToInferMonotFilterDeduction
                        lst
                    )
                    ex
                )
        )
        (currAssumed lst)


tryToInferMonotFilterDeduction : Step -> Maybe { assumed : Maybe Expr, num : Int, what : Expr }
tryToInferMonotFilterDeduction step =
    case step of
        Deduction arg ->
            Just { assumed = arg.assumed, num = arg.num, what = arg.what }

        _ ->
            Nothing


tryToInferRepl : List { number : Int, ex : Expr } -> Expr -> Maybe Reason
tryToInferRepl itms ex =
    let
        res =
            Infer.tryFromReplacement itms ex
    in
    Maybe.andThen
        (\rres ->
            Just (Equivalence { ref = rres.ref, number = rres.which + 1 })
        )
        res


tryToInferImpl : List { number : Int, ex : Expr } -> Expr -> Maybe Reason
tryToInferImpl lst ex =
    Infer.tryFromImplication lst ex |> Maybe.map (\e -> Implication { number = e.what, ref = e.ref })


tryToInferRule : List Step -> Expr -> Maybe Reason
tryToInferRule lst ex =
    Infer.tryFromInferenceRule (List.filterMap tryToInferRuleStep lst) (currAssumed lst) ex
        |> Maybe.map
            (\e ->
                case e.refs of
                    Infer.One ref ->
                        InferenceRule1 { number = e.number + 1, ref1 = ref }

                    Infer.Two ref1 ref2 ->
                        InferenceRule2 { number = e.number + 1, ref1 = ref1, ref2 = ref2 }
            )


tryToInferRuleStep : Step -> Maybe { number : Int, assum : Maybe Expr, what : Expr }
tryToInferRuleStep step =
    case step of
        Deduction ded ->
            Just { number = ded.num, assum = ded.assumed, what = ded.what }

        Assume _ ->
            Nothing


extractInSameTheory : List Step -> List { number : Int, ex : Expr }
extractInSameTheory lst =
    let
        curr =
            currAssumed lst
    in
    List.filterMap
        (\e ->
            case e of
                Deduction arg ->
                    if arg.assumed == curr then
                        Just { number = arg.num, ex = arg.what }

                    else
                        Nothing

                _ ->
                    Nothing
        )
        lst


{-| Get current assumed expression
-}
currAssumed : List Step -> Maybe Expr
currAssumed steps =
    case steps of
        (Deduction arg) :: _ ->
            arg.assumed

        (Assume what) :: _ ->
            what

        [] ->
            Nothing


{-| Get the next number
-}
nextNumber : List Step -> Int
nextNumber lst =
    case currNumber lst of
        Just n ->
            n + 1

        Nothing ->
            1


{-| Get current number
-}
currNumber : List Step -> Maybe Int
currNumber lst =
    case lst of
        (Deduction ded) :: _ ->
            Just ded.num

        (Assume _) :: body ->
            currNumber body

        [] ->
            Nothing



-- The view


view : LevelModel -> Document Msg
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
        [ form [ onSubmit (AddPressed |> LevelMsgV) ]
            [ input
                [ placeholder "Escribe la deducción aquí"
                , value dedtext
                , onInput (\t -> DeductionTextChanged t |> LevelMsgV)
                , class "exercise-bottom-bar-input"
                , type_ "text"
                ]
                []
            , input
                [ type_ "button"
                , class "exercise-bottom-bar-btn"
                , onClick (AddPressed |> LevelMsgV)
                , value "+"
                ]
                []
            , input
                [ type_ "button"
                , class "exercise-bottom-bar-btn"
                , onClick (TheoryPressed |> LevelMsgV)
                , value "T"
                ]
                []
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
                    [ div [ class "clickable", onClick (ExprPressed what |> LevelMsgV) ]
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
        , td [ onClick (ExprPressed ded.what |> LevelMsgV), class "exercise-step-deduction-expression clickable" ]
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
            "E" ++ String.fromInt args.number ++ ":" ++ String.fromInt args.ref

        Implication args ->
            "I" ++ String.fromInt args.number ++ ":" ++ String.fromInt args.ref

        InferenceRule1 args ->
            "R" ++ String.fromInt args.number ++ ":" ++ String.fromInt args.ref1

        InferenceRule2 args ->
            "R" ++ String.fromInt args.number ++ ":" ++ String.fromInt args.ref1 ++ "," ++ String.fromInt args.ref2


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
    div [ class "theory-item clickable", onClick (ExprPressed ex |> LevelMsgV) ] [ MathML.exprToMathML ex ]
