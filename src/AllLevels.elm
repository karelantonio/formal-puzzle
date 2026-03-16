module AllLevels exposing (..)

import Browser exposing (Document)
import Debug exposing (todo)
import Expr exposing (..)
import Html exposing (Html, div, h2, h4, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import LevelTys
import TeaCommon exposing (..)


levels : List Level
levels =
    [ { name = "Primeros Pasos"
      , theory =
            [ And (Ident "p") (Ident "-q")
            , Implies (Ident "-q") (Ident "r")
            ]
      , approx_steps = 3
      }
    ]


update : AllLevelsMsg -> AllLevelsModel -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LevelClicked lvl ->
            ( LevelTys.Ex
                { ded_text = ""
                , error_msg = Nothing
                , theory = lvl.theory
                , steps = [ LevelTys.Assume Nothing ]
                }
                |> LevelModelV
            , Cmd.none
            )


view : AllLevelsModel -> Document Msg
view model =
    case model of
        AllLevelsModel ->
            { title = "Todos los niveles"
            , body =
                [ div [ class "all-levels-ui" ]
                    (h2
                        [ class "all-levels-ui-title" ]
                        [ text "Niveles:" ]
                        :: List.map levelDescription levels
                    )
                ]
            }


levelDescription : Level -> Html Msg
levelDescription lvl =
    div [ class "all-levels-ui-item clickable", onClick (LevelClicked lvl |> AllLevelsMsgV) ]
        [ h4 [ class "all-levels-ui-item-title" ] [ text lvl.name ]
        , p [] [ text ("Cantidad de pasos (aprx.): " ++ String.fromInt lvl.approx_steps) ]
        ]
