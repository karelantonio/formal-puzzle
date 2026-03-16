module TeaCommon exposing (..)

import Expr exposing (Expr)
import LevelTys



-- All Levels


type alias Level =
    { name : String
    , theory : List Expr
    , approx_steps : Int
    }


type AllLevelsMsg
    = LevelClicked Level


type AllLevelsModel
    = AllLevelsModel



-- Messages


type Msg
    = AllLevelsMsgV AllLevelsMsg
    | LevelMsgV LevelTys.LevelMsg



-- Models


type Model
    = AllLevelsModelV AllLevelsModel
    | LevelModelV LevelTys.LevelModel
