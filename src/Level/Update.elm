module Level.Update exposing (subscriptions, update)

import Expr.Parser exposing (parse)
import Expr.Types exposing (Expr(..), Loc(..), toString)
import Infer.Hypothesis
import Infer.InferenceRule
import Infer.Monotony
import Infer.Transformation
import Infer.Types exposing (InferenceRefs(..))
import Level.Types exposing (..)
import Set exposing (Set)
import Utils


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Ex ex ->
            updateEx msg ex


updateEx : Msg -> ExT -> ( Model, Cmd Msg )
updateEx msg ex =
    case msg of
        DeductionTextChanged txt ->
            ( Ex { ex | ded_text = txt, error_msg = Nothing }, Cmd.none )

        AddPressed ->
            case parse ex.domain ex.ded_text of
                Ok parsed_ex ->
                    updateHandleParsed ex parsed_ex

                Err err ->
                    ( Ex
                        { ex
                            | error_msg = err.msg ++ " (Posición: " ++ locationToString err.location ++ ")" |> Just
                        }
                    , Utils.scrollToBottom "exercise-ui"
                    )

        TheoryPressed ->
            if String.isEmpty ex.ded_text then
                ( Ex { ex | steps = addAssumeToSteps Nothing ex.steps }, Cmd.none )

            else
                case parse ex.domain ex.ded_text of
                    Ok parsed_ex ->
                        -- Already checked
                        ( Ex { ex | steps = addAssumeToSteps (Just parsed_ex) ex.steps }, Cmd.none )

                    Err err ->
                        ( Ex { ex | error_msg = err.msg ++ " (Posición: " ++ locationToString err.location ++ ")" |> Just }, Cmd.none )

        ExprPressed what ->
            ( Ex { ex | ded_text = toString what, error_msg = Nothing }, Cmd.none )

        InsertPressed val ->
            ( Ex ex, Utils.insertInInput ( "exercise-bottom-bar-input", val ) )


locationToString : Loc -> String
locationToString loc =
    case loc of
        Pos p ->
            String.fromInt (p + 1)

        End ->
            "al final"


updateHandleParsed : ExT -> Expr -> ( Model, Cmd Msg )
updateHandleParsed ex parsed_ex =
    case tryToInferDed { theory = Debug.log "theory" ex.theory, steps = ex.steps } parsed_ex of
        Just ded ->
            ( Ex
                { ex
                    | ded_text = ""
                    , steps = ded :: ex.steps
                }
            , Utils.scrollToBottom "exercise-content"
            )

        Nothing ->
            ( Ex
                { ex
                    | error_msg = Just "Al parecer no hay regla de inferencia, equivalencia lógica, implicación lógica, monotonía o hipótesis que justifique eso"
                }
            , Utils.scrollToBottom "exercise-ui"
            )



-- Add assume (T, A |-) to the list of steps, if required


addAssumeToSteps : Maybe Expr -> List Step -> List Step
addAssumeToSteps ex steps =
    case steps of
        (Assume _) :: rem ->
            if currAssumed rem == ex && (List.isEmpty rem |> not) then
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
    if Infer.Hypothesis.tryFromHypothesis model.theory (currAssumed model.steps) ex then
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
                (Infer.Monotony.tryFromMonotony
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
    Infer.Transformation.tryFromReplacement itms ex
        |> Maybe.map Equivalence


tryToInferImpl : List { number : Int, ex : Expr } -> Expr -> Maybe Reason
tryToInferImpl lst ex =
    Infer.Transformation.tryFromImplication lst ex
        |> Maybe.map Implication


tryToInferRule : List Step -> Expr -> Maybe Reason
tryToInferRule lst ex =
    Infer.InferenceRule.tryFromInferenceRule (List.filterMap tryToInferRuleStep lst) (currAssumed lst) ex
        |> Maybe.map
            (\e ->
                case e.refs of
                    Infer.Types.OneRef ref ->
                        InferenceRule1 { name = e.name, ref1 = ref }

                    Infer.Types.TwoRefs ref1 ref2 ->
                        InferenceRule2 { name = e.name, ref1 = ref1, ref2 = ref2 }
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



-- Receive messages from outside


subscriptions : Model -> Sub Msg
subscriptions _ =
    Utils.doneInsertInInput DeductionTextChanged
