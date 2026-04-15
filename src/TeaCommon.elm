module TeaCommon exposing (..)

import Expr exposing (Expr)
import LevelTys



-- All Levels


type alias Level =
    { name : String
    , theory : List Expr
    , goal : Expr
    , approx_steps : Int
    , descr : String
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
