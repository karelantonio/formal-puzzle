port module Utils exposing (..)

import Browser exposing (..)


indexed : List a -> List ( Int, a )
indexed l =
    List.indexedMap Tuple.pair l


listProduct : List a -> List b -> List ( a, b )
listProduct l1 l2 =
    case l1 of
        hd :: tail ->
            List.map (\e -> ( hd, e )) l2 ++ listProduct tail l2

        [] ->
            []


indexedProduct : List a -> List b -> List ( ( Int, a ), ( Int, b ) )
indexedProduct l1 l2 =
    listProduct (indexed l1) (indexed l2)


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


toResultAll : List (Result err ok) -> Result err (List ok)
toResultAll lst =
    case lst of
        (Ok v) :: tl ->
            toResultAll tl |> Result.map ((::) v)

        (Err v) :: _ ->
            Err v

        [] ->
            Ok []



-- Given the ID, create a task that scrolls that element to bottom


port scrollToBottom : String -> Cmd msg
