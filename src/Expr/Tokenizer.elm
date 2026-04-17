module Expr.Tokenizer exposing (Token, TokenKind(..), tokenKindToString, tokenize)

import List exposing (drop, head, take)


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
