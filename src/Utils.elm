module Utils exposing (..)


isJust : Maybe a -> Bool
isJust e =
    case e of
        Just _ ->
            True

        Nothing ->
            False


isNothing : Maybe a -> Bool
isNothing e =
    case e of
        Just _ ->
            False

        Nothing ->
            True
