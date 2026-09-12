module Main exposing (..)

import AllLevels.Types exposing (lookupLevelByName)
import AllLevels.Update
import AllLevels.View
import Browser exposing (Document, document)
import Html
import Json.Decode as JD
import Json.Encode as JE
import Level.Types exposing (decodeStep, encodeStep, makeLevel, makeLevelExT)
import Level.Update
import Level.View



-- Messages


type Msg
    = AllLevelsMsgV AllLevels.Types.Msg
    | LevelMsgV Level.Types.Msg



-- Models


type Model
    = AllLevelsModelV AllLevels.Types.Model
    | LevelModelV Level.Types.Model



-- The init function (initial model)


init : JD.Value -> ( Model, Cmd Msg )
init value =
    ( AllLevelsModelV AllLevels.Types.Model, Cmd.none )



-- The update function


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( LevelModelV lvlmod, LevelMsgV lvlmsg ) ->
            Level.Update.update lvlmsg lvlmod |> Tuple.mapBoth LevelModelV (Cmd.map LevelMsgV)

        ( AllLevelsModelV alvlmod, AllLevelsMsgV alvlmsg ) ->
            let
                ans =
                    AllLevels.Update.update alvlmsg alvlmod
            in
            case ans of
                ( AllLevels.Types.ChangeToLevel lvl, _ ) ->
                    ( LevelModelV lvl, Cmd.none )

                other ->
                    Tuple.mapBoth AllLevelsModelV (Cmd.map AllLevelsMsgV) other

        _ ->
            ( model, Cmd.none )



-- The view function


view : Model -> Document Msg
view model =
    case model of
        LevelModelV lvlmod ->
            Level.View.view lvlmod |> mapDoc LevelMsgV

        AllLevelsModelV alvlmod ->
            AllLevels.View.view alvlmod |> mapDoc AllLevelsMsgV


mapDoc : (a -> b) -> Document a -> Document b
mapDoc fn doc =
    { title = doc.title, body = List.map (Html.map fn) doc.body }



-- The subscriptions


subscriptions : Model -> Sub Msg
subscriptions m =
    case m of
        AllLevelsModelV _ ->
            Sub.none

        LevelModelV inst ->
            Level.Update.subscriptions inst |> Sub.map LevelMsgV


{-| Main entry point of the program
-}
main : Program JD.Value Model Msg
main =
    document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| Encoders and decoders for saving the apps state and reloading at startup
-}
encodeSavedLevelState : Level.Types.Model -> JE.Value
encodeSavedLevelState model =
    case model of
        Level.Types.Ex ex ->
            JE.object
                [ ( "name", JE.string ex.lvl )
                , ( "steps", JE.list encodeStep ex.steps )
                ]


decodeSavedLevelState : JD.Decoder Level.Types.Model
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
                    JD.succeed (Level.Types.Ex { ext | steps = steps })

                Nothing ->
                    JD.fail "Unkown name"
        )
        dec
