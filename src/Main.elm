module Main exposing (..)

import Browser exposing (..)
import Expr exposing (..)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, placeholder)


type alias Step =
    { assumed : Maybe Expr, num : Int, what : Expr }


{-| The model
-}
type Model
    = Ex { theory : List Expr, steps : List Step }



-- The message


type Msg
    = Msg



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
        }
    , Cmd.none
    )



-- Update the view


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- The view


view : Model -> Document Msg
view m =
    case m of
        Ex ex ->
            { title = "Ejercicio"
            , body =
                [ div [ class "exercise-ui" ]
                    [ topBar, theory ex.theory ]
                ]
            }


topBar : Html Msg
topBar =
    div
        [ class "exercise-top-bar" ]
        [ input [ placeholder "Escribe la deducción aquí", class "exercise-top-bar-input" ] []
        , button [] [ text "+" ]
        ]


theory : List Expr -> Html Msg
theory lst =
    div [ class "theory" ] (List.map theoryItem lst)


theoryItem : Expr -> Html Msg
theoryItem ex =
    div [ class "theory-item" ] [ Expr.toString ex |> text ]



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
