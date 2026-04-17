module AllLevels.Update exposing (update)

import AllLevels.Types exposing (Model(..), Msg(..))
import Expr.Utils exposing (extractAllKnownPropositions)
import Level.Types


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        LevelClicked lvl ->
            ( Level.Types.Ex
                { ded_text = ""
                , error_msg = Nothing
                , theory = lvl.theory
                , steps = [ Level.Types.Assume Nothing ]
                , goal = lvl.goal
                , descr = lvl.descr
                , known_props = extractAllKnownPropositions lvl.theory
                }
                |> ChangeToLevel
            , Cmd.none
            )
