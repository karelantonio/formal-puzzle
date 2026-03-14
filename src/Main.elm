module Main exposing (..)

import AllRules
import Browser exposing (..)
import Debug exposing (todo)
import Expr exposing (..)
import Html exposing (Html, button, div, form, h2, h3, input, node, table, tbody, td, text, tr)
import Html.Attributes exposing (class, colspan, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import MathML exposing (exprToMathML)


type Step
    = Assume (Maybe Expr)
    | Deduction { assumed : Maybe Expr, num : Int, what : Expr }


{-| The model
-}
type Model
    = Ex { ded_text : String, parse_error : Maybe String, theory : List Expr, steps : List Step }



-- The message


type Msg
    = DeductionTextChanged String
    | AddPressed
    | TheoryPressed
    | TheoryItemPressed Expr



-- Initial state


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Ex
        { theory =
            [ And (Ident "p") (Ident "q")
            , Implies (Ident "p") (Ident "s")
            , Or (Neg (Ident "s")) (Ident "t")
            ]
        , steps = [ Assume Nothing, Deduction { assumed = Nothing, num = 1, what = And (Ident "p") (Ident "q") } ]
        , ded_text = ""
        , parse_error = Nothing
        }
    , Cmd.none
    )



-- Update the view


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Ex ex ->
            updateEx msg ex


updateEx : Msg -> { ded_text : String, parse_error : Maybe String, theory : List Expr, steps : List Step } -> ( Model, Cmd Msg )
updateEx msg ex =
    case msg of
        DeductionTextChanged txt ->
            ( Ex { ex | ded_text = txt, parse_error = Nothing }, Cmd.none )

        AddPressed ->
            case parse ex.ded_text of
                Ok parsed_ex ->
                    todo "Not implemented"

                Err err ->
                    ( Ex { ex | parse_error = err.msg ++ " (Posición: " ++ String.fromInt err.location ++ ")" |> Just }, Cmd.none )

        TheoryPressed ->
            case parse ex.ded_text of
                Ok parsed_ex ->
                    todo "Not implemented"

                Err err ->
                    ( Ex { ex | parse_error = err.msg ++ " (Posición: " ++ String.fromInt err.location ++ ")" |> Just }, Cmd.none )

        TheoryItemPressed what ->
            ( Ex { ex | ded_text = toString what, parse_error = Nothing }, Cmd.none )



-- The view


view : Model -> Document Msg
view m =
    case m of
        Ex ex ->
            { title = "Ejercicio"
            , body =
                [ div [ class "exercise-ui" ]
                    [ topBar ex.ded_text
                    , parseError ex.parse_error
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
        [ table [] [ tbody [] (List.reverse steps |> List.map step2html) ] ]


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
                    [ div [] [ text "T," ]
                    , exprToMathML what
                    ]
                , td [] [ deductionSymbol ]
                , td [ class "exercise-step-deduction-expression" ] [ text "(Nada todavía, haz tus deducciones)" ]
                ]

        Nothing ->
            tr [ class "exercise-step-deduction" ]
                [ td []
                    [ text "T"
                    ]
                , td [] [ deductionSymbol ]
                , td [ class "exercise-step-deduction-expression exercise-step-deduction-nothing" ] [ text "(Nada todavía, haz tus deducciones)" ]
                ]


step2htmlDeduction : { assumed : Maybe Expr, num : Int, what : Expr } -> Html Msg
step2htmlDeduction ded =
    tr [ class "exercise-step-deduction" ]
        [ td [] []
        , td [ class "exercise-step-deduction-symbol" ]
            [ deductionSymbol ]
        , td [ class "exercise-step-deduction-expression" ]
            [ exprToMathML ded.what ]
        , td [ class "exercise-step-deduction-stepnum" ] [ text ("(" ++ String.fromInt ded.num ++ ")") ]
        ]


deductionSymbol : Html Msg
deductionSymbol =
    node "math" [] [ node "mrow" [] [ node "mo" [] [ text "⊢" ] ] ]


theory : List Expr -> Html Msg
theory lst =
    div [ class "theory" ] (h3 [] [ text "Teoría:" ] :: List.map theoryItem lst)


theoryItem : Expr -> Html Msg
theoryItem ex =
    div [ class "theory-item", onClick (TheoryItemPressed ex) ] [ MathML.exprToMathML ex ]



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
