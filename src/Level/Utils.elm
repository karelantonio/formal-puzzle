module Level.Utils exposing (..)

import AllLevels.Utils exposing (lookupLevelByName)
import Json.Decode as JD
import Json.Encode as JE
import Level.Types exposing (Model(..), decodeStep, encodeStep, makeLevelExT)



-- For saving the state


encodeSavedLevelState : Model -> JE.Value
encodeSavedLevelState model =
    case model of
        Ex ex ->
            JE.object
                [ ( "name", JE.string ex.lvl )
                , ( "steps", JE.list encodeStep ex.steps )
                ]


decodeSavedLevelState : JD.Decoder Model
decodeSavedLevelState =
    let
        dec =
            JD.map2 Tuple.pair (JD.field "name" JD.string) (JD.field "steps" <| JD.list decodeStep)
    in
    JD.andThen
        (\( name, steps ) ->
            case lookupLevelByName name of
                Just v ->
                    let
                        ext =
                            makeLevelExT { lvl = v.name, descr = v.descr, goal = v.goal }
                    in
                    JD.succeed (Ex { ext | steps = steps })

                Nothing ->
                    JD.fail "Unkown name"
        )
        dec
