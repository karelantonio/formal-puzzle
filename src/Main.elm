module Main exposing (..)

import AllRules
import Browser exposing (..)
import Debug exposing (todo)
import Expr exposing (..)
import Html exposing (Html, button, div, h2, h3, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onInput)
import MathML


type alias Step =
    { assumed : Maybe Expr, num : Int, what : Expr }


{-| The model
-}
type Model
    = Ex { ded_text : String, theory : List Expr, steps : List Step }



-- The message


type Msg
    = DeductionTextChanged String



-- Initial state


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Ex
        { theory =
            [ And (Ident "p") (Ident "q")
            , Implies (Ident "p") (Ident "s")
            , Or (Neg (Ident "s")) (Ident "t")
            ]
        , steps = []
        , ded_text = ""
        }
    , Cmd.none
    )



-- Update the view


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Ex ex ->
            updateEx msg ex


updateEx : Msg -> { ded_text : String, theory : List Expr, steps : List Step } -> ( Model, Cmd Msg )
updateEx msg ex =
    case msg of
        DeductionTextChanged txt ->
            ( Ex { ex | ded_text = txt }, Cmd.none )



-- The view


view : Model -> Document Msg
view m =
    case m of
        Ex ex ->
            { title = "Ejercicio"
            , body =
                [ div [ class "exercise-ui" ]
                    [ topBar ex.ded_text, theory ex.theory ]
                ]
            }


topBar : String -> Html Msg
topBar dedtext =
    div
        [ class "exercise-top-bar" ]
        [ input
            [ placeholder "Escribe la deducción aquí"
            , value dedtext
            , onInput DeductionTextChanged
            , class "exercise-top-bar-input"
            ]
            []
        , button [ class "exercise-top-bar-btn" ] [ text "+" ]
        , button [ class "exercise-top-bar-btn" ] [ text "T" ]
        ]


theory : List Expr -> Html Msg
theory lst =
    div [ class "theory" ] (h3 [] [ text "Teoría:" ] :: List.map theoryItem lst)


theoryItem : Expr -> Html Msg
theoryItem ex =
    div [ class "theory-item" ] [ MathML.exprToMathML ex ]



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
