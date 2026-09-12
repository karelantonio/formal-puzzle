module AllLevels.Utils exposing (..)

import AllLevels.Types exposing (Level, levels)


lookupLevelByName : String -> Maybe Level
lookupLevelByName name =
    lookupLevelByNameHlpr name levels


lookupLevelByNameHlpr : String -> List Level -> Maybe Level
lookupLevelByNameHlpr lvlname lvls =
    case lvls of
        hd :: tl ->
            if hd.name == lvlname then
                Just hd

            else
                lookupLevelByNameHlpr lvlname tl

        [] ->
            Nothing
