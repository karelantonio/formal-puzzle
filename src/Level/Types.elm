module Level.Types exposing (..)

import Expr.Types exposing (Domain, Expr(..), decodeExpr, encodeExpr, extractDomainFromTheory)
import Html.Attributes exposing (step)
import Json.Decode as JD
import Json.Encode as JE


type Reason
    = Monotony Int
    | Hypotesis
    | Equivalence { name : String, ref : Int }
    | Implication { name : String, ref : Int }
    | InferenceRule1 { name : String, ref1 : Int }
    | InferenceRule2 { name : String, ref1 : Int, ref2 : Int }


type Step
    = Assume (Maybe Expr)
    | Deduction { assumed : Maybe Expr, num : Int, what : Expr, reason : Reason }


type DescrItem
    = Text String
    | Theory Expr


type alias ExT =
    { lvl : String
    , descr : List DescrItem
    , goal : Expr
    , ded_text : String
    , error_msg : Maybe String
    , theory : List Expr
    , domain : Domain
    , steps : List Step
    }


{-| Usen in AllLevels to create the levels
-}
makeLevel : { lvl : String, descr : List DescrItem, goal : Expr } -> Model
makeLevel =
    Ex << makeLevelExT


makeLevelExT : { lvl : String, descr : List DescrItem, goal : Expr } -> ExT
makeLevelExT info =
    let
        theory =
            List.filterMap
                (\e ->
                    case e of
                        Theory i ->
                            Just i

                        _ ->
                            Nothing
                )
                info.descr
    in
    { lvl = info.lvl
    , descr = info.descr
    , goal = info.goal
    , theory = theory
    , ded_text = ""
    , error_msg = Nothing
    , steps = [ Assume Nothing ]
    , domain = extractDomainFromTheory (info.goal :: theory)
    }


type Model
    = Ex ExT


type Msg
    = DeductionTextChanged String
    | AddPressed
    | TheoryPressed
    | ExprPressed Expr
    | InsertPressed String



-- Some encoders / decoders


encodeReason : Reason -> JE.Value
encodeReason r =
    case r of
        Monotony n ->
            JE.object [ ( "mon", JE.int n ) ]

        Hypotesis ->
            JE.object [ ( "hyp", JE.null ) ]

        Equivalence { name, ref } ->
            JE.object
                [ ( "equiv", JE.string name )
                , ( "ref", JE.int ref )
                ]

        Implication { name, ref } ->
            JE.object
                [ ( "impl", JE.string name )
                , ( "ref", JE.int ref )
                ]

        InferenceRule1 { name, ref1 } ->
            JE.object
                [ ( "inf", JE.string name )
                , ( "ref1", JE.int ref1 )
                ]

        InferenceRule2 { name, ref1, ref2 } ->
            JE.object
                [ ( "inf2", JE.string name )
                , ( "ref1", JE.int ref1 )
                , ( "ref2", JE.int ref2 )
                ]


decodeReason : JD.Decoder Reason
decodeReason =
    JD.oneOf
        [ JD.field "mon" JD.int |> JD.map Monotony
        , JD.field "hyp" JD.value |> JD.map (\_ -> Hypotesis)
        , JD.map2 (\n r -> Equivalence { name = n, ref = r })
            (JD.field "equiv" JD.string)
            (JD.field "ref" JD.int)
        , JD.map2 (\n r -> Implication { name = n, ref = r })
            (JD.field "impl" JD.string)
            (JD.field "ref" JD.int)
        , JD.map2 (\n r -> InferenceRule1 { name = n, ref1 = r })
            (JD.field "inf" JD.string)
            (JD.field "ref1" JD.int)
        , JD.map3 (\n r r2 -> InferenceRule2 { name = n, ref1 = r, ref2 = r2 })
            (JD.field "inf2" JD.string)
            (JD.field "ref1" JD.int)
            (JD.field "ref2" JD.int)
        ]


encodeStep : Step -> JE.Value
encodeStep step =
    case step of
        Assume Nothing ->
            JE.object [ ( "assume", JE.null ) ]

        Assume (Just ex) ->
            JE.object [ ( "assume", encodeExpr ex ) ]

        Deduction { assumed, num, what, reason } ->
            JE.object
                [ ( "deduction"
                  , JE.object
                        [ ( "assumed", Maybe.map encodeExpr assumed |> Maybe.withDefault JE.null )
                        , ( "num", JE.int num )
                        , ( "what", encodeExpr what )
                        , ( "reason", encodeReason reason )
                        ]
                  )
                ]


decodeStep : JD.Decoder Step
decodeStep =
    JD.oneOf
        [ JD.field "assume" (JD.maybe decodeExpr) |> JD.map Assume
        , JD.field "deduction"
            (JD.map4
                (\a n w r -> Deduction { assumed = a, num = n, what = w, reason = r })
                (JD.field "assumed" (JD.maybe decodeExpr))
                (JD.field "num" JD.int)
                (JD.field "what" decodeExpr)
                (JD.field "reason" decodeReason)
            )
        ]
