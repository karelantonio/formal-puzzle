module Combinators exposing (Addr(..), Error, Recognizer, accept, chain, chain3, choice, choice3, empty, end, expect, lazy, map, maybe, maybeDefault)


type Addr
    = Pos Int
    | End


type alias Error =
    { addr : Addr, expected : List String }


type alias Recognizer val tok =
    (tok -> Int) -> List tok -> Result Error ( val, List tok )



-- Match the empty data


empty : Recognizer () tok
empty _ l =
    Ok ( (), l )



-- Match the end of the data


end : Recognizer () tok
end addr l =
    case l of
        hd :: _ ->
            Err { addr = addr hd |> Pos, expected = [] }

        [] ->
            Ok ( (), [] )



-- Match if the function accepts the value


accept : (tok -> Maybe b) -> Recognizer b tok
accept fn addr lst =
    case lst of
        hd :: tl ->
            case fn hd of
                Just v ->
                    Ok ( v, tl )

                Nothing ->
                    Err { addr = Pos <| addr hd, expected = [] }

        [] ->
            Err { addr = End, expected = [] }



-- Chain two recognizer together


chain : (a -> b -> c) -> Recognizer a tok -> Recognizer b tok -> Recognizer c tok
chain fn r1 r2 addr lst =
    -- This could be Result.andThen but that would be too ugly
    case r1 addr lst of
        Ok ( a, lst2 ) ->
            case r2 addr lst2 of
                Ok ( b, lst3 ) ->
                    Ok ( fn a b, lst3 )

                Err e ->
                    Err e

        Err e ->
            Err e


chain3 : (a -> b -> c -> d) -> Recognizer a tok -> Recognizer b tok -> Recognizer c tok -> Recognizer d tok
chain3 fn r1 r2 r3 addr lst =
    case r1 addr lst of
        Ok ( a, lst2 ) ->
            case r2 addr lst2 of
                Ok ( b, lst3 ) ->
                    case r3 addr lst3 of
                        Ok ( c, lst4 ) ->
                            Ok ( fn a b c, lst4 )

                        Err e ->
                            Err e

                Err e ->
                    Err e

        Err e ->
            Err e



-- Choice between two recognizers (takes the farthest if both match, and takaes the farthest error if both fail)


choice : Recognizer a tok -> Recognizer a tok -> Recognizer a tok
choice r1 r2 addr lst =
    case ( r1 addr lst, r2 addr lst ) of
        ( v1, v2 ) ->
            farthest v1 v2 (toAddr addr v1) (toAddr addr v2)


toAddr : (tk -> Int) -> Result Error ( b, List tk ) -> Addr
toAddr fn res =
    case res of
        Ok ( _, v ) ->
            case v of
                hd :: _ ->
                    Pos <| fn hd

                [] ->
                    End

        Err err ->
            err.addr


farthest : Result x a -> Result x a -> Addr -> Addr -> Result x a
farthest v1 v2 a1 a2 =
    case ( a1, a2 ) of
        ( End, End ) ->
            getTheOk v1 v2

        ( End, _ ) ->
            v1

        ( _, End ) ->
            v2

        ( Pos p1, Pos p2 ) ->
            if p1 > p2 then
                v1

            else if p1 == p2 then
                getTheOk v1 v2

            else
                v2


getTheOk : Result x a -> Result x a -> Result x a
getTheOk v1 v2 =
    case v1 of
        Ok v ->
            Ok v

        Err _ ->
            v2


choice3 : Recognizer a tok -> Recognizer a tok -> Recognizer a tok -> Recognizer a tok
choice3 r1 r2 r3 =
    choice r1 (choice r2 r3)



-- Maybe a recognizer (A | epsilon)


maybe : Recognizer a tok -> Recognizer (Maybe a) tok
maybe rec addr lst =
    case rec addr lst of
        Ok v ->
            Tuple.mapFirst Just v |> Ok

        Err _ ->
            Ok ( Nothing, lst )


maybeDefault : a -> Recognizer a tok -> Recognizer a tok
maybeDefault def rec addr lst =
    rec addr lst |> Result.withDefault ( def, lst ) |> Ok



-- Map the result of a recognizer


map : (a -> b) -> Recognizer a tok -> Recognizer b tok
map fn rec addr lst =
    rec addr lst |> Result.map (Tuple.mapFirst fn)



-- Expect the given recognizer to match (orelse, add the name to the expected)


expect : String -> Recognizer a tok -> Recognizer a tok
expect ex rec addr lst =
    rec addr lst |> Result.mapError (\e -> { e | expected = ex :: e.expected })



-- A lazy recognizer


lazy : (() -> Recognizer a tok) -> Recognizer a tok
lazy init addr lst =
    init () addr lst
