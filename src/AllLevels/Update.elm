module AllLevels.Update exposing (update)

import AllLevels.Types exposing (Model(..), Msg(..))
import Expr.Types exposing (extractDomainFromTheory)
import Level.Types exposing (makeLevel)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        LevelClicked lvl ->
            ( makeLevel { descr = lvl.descr, goal = lvl.goal }
                |> ChangeToLevel
            , Cmd.none
            )
