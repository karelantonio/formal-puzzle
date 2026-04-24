module AllLevels.Types exposing (..)

import Expr.Types exposing (Expr(..), FunTree(..))
import Html exposing (Html, div, h4, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Level.Types exposing (DescrItem(..))



-- All Levels


type alias Level =
    { name : String
    , goal : Expr
    , approx_steps : Int
    , descr : List DescrItem
    }


type Msg
    = LevelClicked Level


type Model
    = Model
    | ChangeToLevel Level.Types.Model


levels : List Level
levels =
    [ { name = "Primeros Pasos"
      , goal = Ident "r"
      , descr =
            [ Text "El objetivo de este nivel es que aprendas a usar la interfaz."
            , Text "Abajo te encontrarás la ENTRADA, ahí vas a escribir tus deducciones tal como lo harías en un mensaje de Whatsapp. Para escribir conjunciones debes usar usar el operador &, para escribir disyunciones el operador |, para implicaciones y doble implicaciones -> y <-> respectivamente. También puedes usar paréntesis."
            , Text "Cuando termines de escribir tu fórmula presiona (+), si es correcta y hay alguna regla de inferencia que lo justifique se añadirá a los pasos."
            , Text "A continuación esta la 'Teoría' (cosas que puedes asumir):"
            , Theory (And (Ident "p") (Ident "q" |> Neg))
            , Theory (Implies (Ident "q" |> Neg) (Ident "r"))
            , Text "Si presionas cualquiera de esos elementos de arriba se escribirán automaticamente abajo."
            , Text "Intenta escribir 'p&-q'. Notarás que también está el botón (T), eso sirve para asumir cosas en tu teoría (T,A |- P), si quieres dejar de asumir en la teoría simplemente deja vacía la ENTRADA y presiona (T). Intenta resolver este nivel sencillo!"
            ]
      , approx_steps = 3
      }
    , { name = "Pollos"
      , descr =
            [ Text "En la isla de los pollos, donde todos siempre dicen la verdad o mienten al menos una vez, ha surgido un dilema, donde Adán (A) dice: hay alguien entre Papote (P), Sergio (S) y yo (A) que dice mentiras. Los detalles no importan pero ahora ellos se empeñan en demostrar que si Sergio (S) y Papote (P) dicen la verdad entonces Adán (A) miente."
            , Text "Las expresiones A, P y S representan: Adán, Papote y Sergio dicen la verdad respectivamente."
            , Theory <| Implies (Ident "A") (Or (Neg (Ident "A")) (Or (Neg (Ident "P")) (Neg (Ident "S"))))
            ]
      , goal = Implies (And (Ident "S") (Ident "P")) (Neg (Ident "A"))
      , approx_steps = 8
      }
    , { name = "Suma de pares"
      , descr =
            [ Text "Todos saben que la suma de un número par con otro número par es par, pero entre saberlo y demostrarlo hay una gran diferencia."
            , Text "Decimos que un número 'x' es par, sí y solo sí existe otro 'y' tal que y+y=x, en otras palabras:"
            , Theory <| Forall "x" (Iff (Predicate "par" [ Atom "x" ]) (Exists "y" (Predicate "=" [ Atom "x", Apply "s" [ Atom "y", Atom "y" ] ])))
            , Text "Donde s(a,b) es la suma de a y b. Y es una función que cumple los siguientes axiomas:"
            , Theory <|
                Forall "a" <|
                    Forall "b" <|
                        Forall "c" <|
                            Forall "d" <|
                                Implies (And (Predicate "=" [ Atom "a", Atom "c" ]) (Predicate "=" [ Atom "b", Atom "d" ]))
                                    (Predicate "=" [ Apply "s" [ Atom "a", Atom "b" ], Apply "s" [ Atom "c", Atom "d" ] ])
            , Theory <|
                Forall "x" <|
                    Forall "y" <|
                        Exists "z" <|
                            Predicate "=" [ Apply "s" [ Atom "x", Atom "y" ], Atom "z" ]
            , Theory <|
                Forall "a" <|
                    Forall "b" <|
                        Predicate "=" [ Apply "s" [ Atom "a", Atom "b" ], Apply "s" [ Atom "b", Atom "a" ] ]
            , Text "Además, junto con la función sucesor cumplen:"
            , Theory <|
                Forall "x" <|
                    Forall "y" <|
                        Predicate "=" [ Apply "s" [ Atom "x", Apply "suc" [ Atom "y" ] ], Apply "suc" [ Apply "s" [ Atom "x", Atom "y" ] ] ]
            , Theory <|
                Forall "x" <|
                    Exists "y" <|
                        Predicate "=" [ Apply "suc" [ Atom "x" ], Atom "y" ]
            , Theory <|
                Neg <|
                    Exists "x" <|
                        Predicate "=" [ Apply "suc" [ Atom "x" ], Atom "0" ]
            , Theory <|
                Forall "x" <|
                    Forall "y" <|
                        Implies (Predicate "=" [ Atom "x", Atom "y" ]) (Predicate "=" [ Apply "suc" [ Atom "x" ], Apply "suc" [ Atom "y" ] ])
            ]
      , goal =
            Forall "x" <|
                Forall "y" <|
                    Implies (And (Predicate "par" [ Atom "x" ]) (Predicate "par" [ Atom "y" ])) (Predicate "par" [ Apply "s" [ Atom "x", Atom "y" ] ])
      , approx_steps = 2
      }
    ]


levelDescription : Level -> Html Msg
levelDescription lvl =
    div [ class "all-levels-ui-item clickable", onClick (LevelClicked lvl) ]
        [ h4 [ class "all-levels-ui-item-title" ] [ text lvl.name ]
        , p [] [ text ("Cantidad de pasos (aprx.): " ++ String.fromInt lvl.approx_steps) ]
        ]
