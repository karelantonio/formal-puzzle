module AllLevels.View exposing (view)

import AllLevels.Types exposing (Model(..), Msg, levelDescription, levels)
import Browser exposing (Document)
import Html exposing (div, h2, text)
import Html.Attributes exposing (class)


view : Model -> Document Msg
view model =
    case model of
        Model ->
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

        ChangeToLevel _ ->
            { title = "", body = [] }
