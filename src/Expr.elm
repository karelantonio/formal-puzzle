module Expr exposing (Expr(..), ParseError, parse, toString)

{-| An expression
-}

import List exposing (drop, head, take)


type Expr
    = One
    | Zero
    | Ident String
    | Neg Expr
    | And Expr Expr
    | Or Expr Expr
    | Implies Expr Expr
    | Iff Expr Expr


{-| Check if an expression is simple
-}
isSimple : Expr -> Bool
isSimple expr =
    case expr of
        One ->
            True

        Zero ->
            True

        Ident _ ->
            True

        Neg _ ->
            True

        And _ _ ->
            False

        Or _ _ ->
            False

        Implies _ _ ->
            False

        Iff _ _ ->
            False


{-| Convert an expression to string
-}
toString : Expr -> String
toString expr =
    case expr of
        One ->
            "1"

        Zero ->
            "0"

        Ident id ->
            id

        Neg sub ->
            "-" ++ parToString sub

        And left right ->
            parToString left ++ "&" ++ parToString right

        Or left right ->
            parToString left ++ "|" ++ parToString right

        Implies left right ->
            parToString left ++ "->" ++ parToString right

        Iff left right ->
            parToString left ++ "<->" ++ parToString right


{-| Add parenthesis around if is not simple
-}
parToString : Expr -> String
parToString expr =
    if isSimple expr then
        toString expr

    else
        "(" ++ toString expr ++ ")"


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
                    Ok ( One, body )

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


{-| A token
-}
type alias Token =
    { pos : Int, kind : TokenKind }


type TokenKind
    = TokLPar
    | TokRPar
    | TokLSqBrac
    | TokRSqBrac
    | TokLBrac
    | TokRBrac
    | TokNum String
    | TokNot
    | TokAnd
    | TokOr
    | TokImplies
    | TokIff
    | TokIdent String
    | TokUnknown String


tokenKindToString : TokenKind -> String
tokenKindToString k =
    case k of
        TokLPar ->
            "("

        TokRPar ->
            ")"

        TokLBrac ->
            "{"

        TokRBrac ->
            "}"

        TokLSqBrac ->
            "["

        TokRSqBrac ->
            "]"

        TokNum v ->
            "Numeric value (" ++ v ++ ")"

        TokNot ->
            "-"

        TokAnd ->
            "&"

        TokOr ->
            "|"

        TokImplies ->
            "->"

        TokIff ->
            "<->"

        TokIdent id ->
            "Identifier (" ++ id ++ ")"

        TokUnknown c ->
            "Unkown (" ++ c ++ ")"


tokenize : Int -> List Char -> List Token
tokenize idx s =
    case s of
        head :: body ->
            if head == '(' then
                { pos = idx, kind = TokLPar } :: tokenize (idx + 1) body

            else if head == ')' then
                { pos = idx, kind = TokRPar } :: tokenize (idx + 1) body

            else if head == '{' then
                { pos = idx, kind = TokLBrac } :: tokenize (idx + 1) body

            else if head == '}' then
                { pos = idx, kind = TokRBrac } :: tokenize (idx + 1) body

            else if head == '[' then
                { pos = idx, kind = TokLSqBrac } :: tokenize (idx + 1) body

            else if head == ']' then
                { pos = idx, kind = TokRSqBrac } :: tokenize (idx + 1) body

            else if head == '-' && take 1 body /= [ '>' ] then
                { pos = idx, kind = TokNot } :: tokenize (idx + 1) body

            else if head == '&' then
                { pos = idx, kind = TokAnd } :: tokenize (idx + 1) body

            else if head == '|' then
                { pos = idx, kind = TokOr } :: tokenize (idx + 1) body

            else if head == '-' && take 1 body == [ '>' ] then
                { pos = idx, kind = TokImplies } :: tokenize (idx + 2) (drop 1 body)

            else if head == '<' && take 2 body == [ '-', '>' ] then
                { pos = idx, kind = TokIff } :: tokenize (idx + 4) (drop 2 body)

            else if head == ' ' || head == '\t' || head == '\n' || head == '\u{000D}' then
                tokenize (idx + 1) body

            else if Char.isDigit head then
                let
                    red =
                        reduceWith Char.isDigit s
                in
                { pos = idx, kind = TokNum red.value } :: tokenize (idx + String.length red.value) red.tail

            else if Char.isAlpha head then
                let
                    red =
                        reduceWith Char.isAlphaNum s
                in
                { pos = idx, kind = TokIdent red.value } :: tokenize (idx + String.length red.value) red.tail

            else
                [ { pos = idx, kind = TokUnknown ("Unexpected: " ++ String.fromChar head) } ]

        _ ->
            []


reduceWith : (Char -> Bool) -> List Char -> { value : String, tail : List Char }
reduceWith fn lst =
    case lst of
        head :: body ->
            if fn head then
                let
                    sub =
                        reduceWith fn body
                in
                { sub | value = String.fromChar head ++ sub.value }

            else
                { value = "", tail = lst }

        _ ->
            { value = "", tail = lst }
