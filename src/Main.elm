module Main exposing (..)

import AllLevels.Types
import AllLevels.Update
import AllLevels.View
import Browser exposing (Document, document)
import Html
import Level.Types
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


init : flags -> ( Model, Cmd Msg )
init _ =
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
