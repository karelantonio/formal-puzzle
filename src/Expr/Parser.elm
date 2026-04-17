module Expr.Parser exposing (ParseError, parse)

import Expr.Tokenizer exposing (..)
import Expr.Types exposing (..)


{-| The location and cause of the error
-}
type alias ParseError =
    { location : Int, msg : String }


{-| Parse an expression
-}
parse : String -> Result ParseError Expr
parse s =
    let
        toks =
            tokenize 0 (String.toList s)
    in
    case toks of
        [] ->
            Err { location = 0, msg = "Expression is empty" }

        _ ->
            case parseIff toks of
                Ok ( ex, [] ) ->
                    Ok ex

                Ok ( _, head :: _ ) ->
                    Err { location = head.pos, msg = "Expected ->, <->, &, | or EOF, but found: " ++ tokenKindToString head.kind }

                Err err ->
                    Err err


parseIff : List Token -> Result ParseError ( Expr, List Token )
parseIff lst =
    parseBinOp (\t -> t.kind == TokIff) Iff parseImplies lst


parseImplies : List Token -> Result ParseError ( Expr, List Token )
parseImplies lst =
    parseBinOp (\t -> t.kind == TokImplies) Implies parseOr lst


parseOr : List Token -> Result ParseError ( Expr, List Token )
parseOr lst =
    parseBinOp (\t -> t.kind == TokOr) Or parseAnd lst


parseAnd : List Token -> Result ParseError ( Expr, List Token )
parseAnd lst =
    parseBinOp (\t -> t.kind == TokAnd) And parseNeg lst


parseNeg : List Token -> Result ParseError ( Expr, List Token )
parseNeg lst =
    case lst of
        tok :: tail ->
            if tok.kind == TokNot then
                parseNeg tail |> Result.map (\( ex, rem ) -> ( Neg ex, rem ))

            else
                parseAtom lst

        [] ->
            parseAtom lst


parseAtom : List Token -> Result ParseError ( Expr, List Token )
parseAtom lst =
    case lst of
        tok :: body ->
            case tok.kind of
                TokIdent name ->
                    Ok ( Ident name, body )

                TokNum "1" ->
                    Ok ( One, body )

                TokNum "0" ->
                    Ok ( Zero, body )

                TokLPar ->
                    parseAtomPar TokRPar body

                TokLBrac ->
                    parseAtomPar TokRBrac body

                TokLSqBrac ->
                    parseAtomPar TokRSqBrac body

                knd ->
                    Err { location = tok.pos, msg = "Expected IDENTIFIER, 1, 0, (, [, {, -, but found: " ++ tokenKindToString knd }

        _ ->
            Err { location = 0, msg = "Expected IDENTIFIER, 0, 1, (, [, {, -, but found EOF" }


parseAtomPar : TokenKind -> List Token -> Result ParseError ( Expr, List Token )
parseAtomPar rpar lst =
    case parseIff lst of
        Ok ( ex, { pos, kind } :: tail ) ->
            if kind == rpar then
                Ok ( ex, tail )

            else
                Err { location = pos, msg = "Expected " ++ tokenKindToString rpar ++ " but found " ++ tokenKindToString kind }

        Ok ( _, [] ) ->
            Err { location = 0, msg = "Expected " ++ tokenKindToString rpar ++ " but found EOF" }

        Err err ->
            Err err


parseBinOp : (Token -> Bool) -> (Expr -> Expr -> Expr) -> (List Token -> Result ParseError ( Expr, List Token )) -> List Token -> Result ParseError ( Expr, List Token )
parseBinOp op bld fn lst =
    case fn lst of
        Ok ( left, rem ) ->
            case rem of
                head :: body ->
                    if op head then
                        parseBinOp op bld fn body
                            |> Result.map
                                (\( right, remrem ) -> ( bld left right, remrem ))

                    else
                        Ok ( left, rem )

                _ ->
                    Ok ( left, rem )

        Err err ->
            Err err
