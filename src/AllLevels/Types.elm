module AllLevels.Types exposing (..)

import Expr.Types exposing (Expr(..), FunTree(..))
import Html exposing (Html, div, h4, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Level.Types



-- All Levels


type alias Level =
    { name : String
    , theory : List Expr
    , goal : Expr
    , approx_steps : Int
    , descr : String
    }


type Msg
    = LevelClicked Level


type Model
    = Model
    | ChangeToLevel Level.Types.Model


levels : List Level
levels =
    [ { name = "Primeros Pasos"
      , theory =
            [ And (Ident "p") (Ident "q" |> Neg)
            , Implies (Ident "q" |> Neg) (Ident "r")
            ]
      , goal = Ident "r"
      , descr = "El objetivo de este nivel es que aprendas a usar la interfaz. Abajo te encontrarás la ENTRADA, ahí vas a escribir tus deducciones tal como lo harías en un mensaje de Whatsapp. Para escribir conjunciones debes usar usar el operador &, para escribir disyunciones el operador |, para implicaciones y doble implicaciones -> y <-> respectivamente. También puedes usar paréntesis. Cuando termines de escribir tu fórmula presiona (+), si es correcta y hay alguna regla de inferencia que lo justifique se añadirá a los pasos. Intenta escribir 'p&-q'. Notarás que también está el botón (T), eso sirve para asumir cosas en tu teoría (T,A |- P), si quieres dejar de asumir en la teoría simplemente deja vacía la ENTRADA y presiona (T). Intenta resolver este nivel sencillo!"
      , approx_steps = 3
      }
    , { name = "Pollos"
      , theory = [ Implies (Ident "A") (Or (Neg (Ident "A")) (Or (Neg (Ident "P")) (Neg (Ident "S")))) ]
      , goal = Implies (And (Ident "S") (Ident "P")) (Neg (Ident "A"))
      , descr = "En la isla de los pollos, donde todos siempre dicen la verdad o mienten al menos una vez, ha surgido un dilema, donde Adán (A) dice: hay alguien entre Papote (P), Sergio (S) y yo (A) que dice mentiras. Los detalles no importan pero ahora ellos se empeñan en demostrar que si Sergio (S) y Papote (P) dicen la verdad entonces Adán (A) miente. Las expresiones A, P y S representan: Adán, Papote y Sergio dicen la verdad respectivamente."
      , approx_steps = 8
      }
    , { name = "Cuantificadores"
      , theory = [ Forall "x" (Predicate "P" [ Atom "x" ]) ]
      , goal = Predicate "P" [ Atom "1" ]
      , descr = "Esto solo es para probar los predicados"
      , approx_steps = 2
      }
    ]


levelDescription : Level -> Html Msg
levelDescription lvl =
    div [ class "all-levels-ui-item clickable", onClick (LevelClicked lvl) ]
        [ h4 [ class "all-levels-ui-item-title" ] [ text lvl.name ]
        , p [] [ text ("Cantidad de pasos (aprx.): " ++ String.fromInt lvl.approx_steps) ]
        ]
