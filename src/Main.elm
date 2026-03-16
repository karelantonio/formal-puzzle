module Main exposing (..)

import AllLevels
import Browser exposing (Document, document)
import Level
import TeaCommon exposing (..)



-- The init function (initial model)


init : flags -> ( Model, Cmd Msg )
init _ =
    ( AllLevelsModelV AllLevelsModel, Cmd.none )



-- The update function


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( LevelModelV lvlmod, LevelMsgV lvlmsg ) ->
            Level.update lvlmsg lvlmod

        ( AllLevelsModelV alvlmod, AllLevelsMsgV alvlmsg ) ->
            AllLevels.update alvlmsg alvlmod

        _ ->
            ( model, Cmd.none )



-- The view function


view : Model -> Document Msg
view model =
    case model of
        LevelModelV lvlmod ->
            Level.view lvlmod

        AllLevelsModelV alvlmod ->
            AllLevels.view alvlmod



-- The subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


{-| Main entry point of the program
-}
main : Program () Model Msg
main =
    document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
