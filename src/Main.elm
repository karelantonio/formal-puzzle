module Main exposing (..)

import Browser exposing (..)
import Debug exposing (todo)
import Expr exposing (..)
import Html exposing (Html, div, form, h3, input, node, table, tbody, td, text, tr)
import Html.Attributes exposing (class, colspan, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Infer exposing (Transformation(..))
import MathML exposing (exprToMathML)


type Reason
    = Monotony Int
    | Hypotesis
    | Equivalence { number : Int, ref : Int }
    | Implication { number : Int, ref : Int }
    | InferenceRule1 { number : Int, ref1 : Int }
    | InferenceRule2 { number : Int, ref1 : Int, ref2 : Int }


type Step
    = Assume (Maybe Expr)
    | Deduction { assumed : Maybe Expr, num : Int, what : Expr, reason : Reason }


{-| The model
-}
type Model
    = Ex { ded_text : String, error_msg : Maybe String, theory : List Expr, steps : List Step }



-- The message


type Msg
    = DeductionTextChanged String
    | AddPressed
    | TheoryPressed
    | ExprPressed Expr



-- Initial state


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Ex
        { theory =
            [ And (Ident "p") (Ident "q")
            , Implies (Ident "p") (Ident "s")
            , Or (Neg (Ident "s")) (Ident "t")
            , And (Implies (Ident "a") (Ident "b")) (Implies (Ident "b") (Ident "c"))
            ]
        , steps =
            [ Deduction
                { assumed = Nothing
                , num = 1
                , what = And (Ident "p") (Ident "q")
                , reason = Hypotesis
                }
            , Assume Nothing
            ]
        , ded_text = ""
        , error_msg = Nothing
        }
    , Cmd.none
    )



-- Update the view


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Ex ex ->
            updateEx msg ex


updateEx : Msg -> { ded_text : String, error_msg : Maybe String, theory : List Expr, steps : List Step } -> ( Model, Cmd Msg )
updateEx msg ex =
    case msg of
        DeductionTextChanged txt ->
            ( Ex { ex | ded_text = txt, error_msg = Nothing }, Cmd.none )

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
                            , Cmd.none
                            )

                        Nothing ->
                            ( Ex { ex | error_msg = Just "Al parecer no hay regla de inferencia, equivalencia lógica, implicación lógica, monotonía o hipótesis que justifique eso" }, Cmd.none )

                Err err ->
                    ( Ex { ex | error_msg = err.msg ++ " (Posición: " ++ String.fromInt (err.location + 1) ++ ")" |> Just }, Cmd.none )

        TheoryPressed ->
            if String.isEmpty ex.ded_text then
                ( Ex { ex | steps = addAssumeToSteps Nothing ex.steps }, Cmd.none )

            else
                case parse ex.ded_text of
                    Ok parsed_ex ->
                        ( Ex { ex | steps = addAssumeToSteps (Just parsed_ex) ex.steps }, Cmd.none )

                    Err err ->
                        ( Ex { ex | error_msg = err.msg ++ " (Posición: " ++ String.fromInt err.location ++ ")" |> Just }, Cmd.none )

        ExprPressed what ->
            ( Ex { ex | ded_text = toString what, error_msg = Nothing }, Cmd.none )


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
                                todo "Not implemented"


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


view : Model -> Document Msg
view m =
    case m of
        Ex ex ->
            { title = "Ejercicio"
            , body =
                [ div [ class "exercise-ui" ]
                    [ topBar ex.ded_text
                    , parseError ex.error_msg
                    , revSteps ex.steps
                    , theory ex.theory
                    ]
                ]
            }


topBar : String -> Html Msg
topBar dedtext =
    div
        [ class "exercise-top-bar" ]
        [ form [ onSubmit AddPressed ]
            [ input
                [ placeholder "Escribe la deducción aquí"
                , value dedtext
                , onInput DeductionTextChanged
                , class "exercise-top-bar-input"
                , type_ "text"
                ]
                []
            , input
                [ type_ "button"
                , class "exercise-top-bar-btn"
                , onClick AddPressed
                , value "+"
                ]
                []
            , input
                [ type_ "button"
                , class "exercise-top-bar-btn"
                , onClick TheoryPressed
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


revSteps : List Step -> Html Msg
revSteps steps =
    div [ class "exercise-ui-steps" ]
        [ table [] [ tbody [] (List.map step2html steps) ] ]


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


theory : List Expr -> Html Msg
theory lst =
    div [ class "theory" ] (h3 [] [ text "Teoría:" ] :: List.map theoryItem lst)


theoryItem : Expr -> Html Msg
theoryItem ex =
    div [ class "theory-item clickable", onClick (ExprPressed ex) ] [ MathML.exprToMathML ex ]



-- The subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


{-| Main entry point of the program
-}
main : Program () Model Msg
main =
    document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
