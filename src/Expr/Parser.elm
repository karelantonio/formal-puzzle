module Expr.Parser exposing (parse)

import Dict
import Expr.Tokenizer exposing (..)
import Expr.Types exposing (..)
import Set exposing (Set)


{-| Parse an expression
-}
parse : Domain -> String -> Result ParseError Expr
parse domain s =
    parseNoCheckPred s |> Result.andThen (checkMapping domain)


checkMapping : Domain -> Expr -> Result ParseError Expr
checkMapping domain ex =
    checkPredicate domain Set.empty ex |> Result.mapError (\msg -> { location = 0, msg = msg })


parseNoCheckPred : String -> Result ParseError Expr
parseNoCheckPred s =
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
    parseBinOp (\t -> t.kind == TokAnd) And parseQuantifier lst


parseQuantifier : List Token -> Result ParseError ( Expr, List Token )
parseQuantifier lst =
    case lst of
        hd :: tl ->
            case hd.kind of
                TokForall ->
                    parseQuantifierArg Forall tl

                TokExists ->
                    parseQuantifierArg Exists tl

                _ ->
                    parseNeg lst

        [] ->
            parseNeg lst


parseQuantifierArg : (String -> Expr -> Expr) -> List Token -> Result ParseError ( Expr, List Token )
parseQuantifierArg fn lst =
    case lst of
        lpar :: name :: rpar :: tl ->
            case ( lpar.kind, name.kind, rpar.kind ) of
                ( TokLPar, TokIdent x, TokRPar ) ->
                    parseQuantifier tl |> Result.map (Tuple.mapFirst (fn x))

                ( TokLBrac, TokIdent x, TokRBrac ) ->
                    parseQuantifier tl |> Result.map (Tuple.mapFirst (fn x))

                ( TokLPar, TokNum x, TokRPar ) ->
                    parseQuantifier tl |> Result.map (Tuple.mapFirst (fn x))

                ( TokLBrac, TokNum x, TokRBrac ) ->
                    parseQuantifier tl |> Result.map (Tuple.mapFirst (fn x))

                _ ->
                    Err { location = lpar.pos, msg = "Expected: (NAME)" }

        _ ->
            Err { location = 0, msg = "Expected: (NAME)" }


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
                    -- It may be a predicate
                    case body of
                        lpar :: tl ->
                            if lpar.kind == TokLPar || lpar.kind == TokLBrac || lpar.kind == TokLSqBrac then
                                parseFunTreeArgsPar (lpar :: tl) |> Result.map (Tuple.mapFirst (Predicate name))

                            else
                                Ok ( Ident name, body )

                        [] ->
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
                    Err { location = tok.pos, msg = "Expected IDENTIFIER, 1, 0, (, [, {, -, ∀, ∃, but found: " ++ tokenKindToString knd }

        _ ->
            Err { location = 0, msg = "Expected IDENTIFIER, 0, 1, (, [, {, -, ∀, ∃, but found EOF" }


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


parseFunTree : List Token -> Result ParseError ( FunTree, List Token )
parseFunTree lst =
    case lst of
        { kind, pos } :: tl ->
            case kind of
                TokIdent name ->
                    case tl of
                        snd :: _ ->
                            if snd.kind == TokLPar || snd.kind == TokLBrac || snd.kind == TokLSqBrac then
                                parseFunTreeArgsPar tl |> Result.map (Tuple.mapFirst (Apply name))

                            else
                                Ok ( Atom name, tl )

                        _ ->
                            Ok ( Atom name, tl )

                TokNum name ->
                    case tl of
                        snd :: _ ->
                            if snd.kind == TokLPar || snd.kind == TokLBrac || snd.kind == TokLSqBrac then
                                parseFunTreeArgsPar tl |> Result.map (Tuple.mapFirst (Apply name))

                            else
                                Ok ( Atom name, tl )

                        _ ->
                            Ok ( Atom name, tl )

                _ ->
                    Err { location = pos, msg = "Expected NAME, but found " ++ tokenKindToString kind }

        [] ->
            Err { location = 0, msg = "Expected NAME, but found EOF" }


parseFunTreeArgsPar : List Token -> Result ParseError ( List FunTree, List Token )
parseFunTreeArgsPar lst =
    case lst of
        hd :: tl ->
            if hd.kind == TokLPar then
                parseFunTreeArgs tl
                    |> Result.andThen (parseFunParExpected TokRPar)

            else if hd.kind == TokLBrac then
                parseFunTreeArgs tl
                    |> Result.andThen (parseFunParExpected TokRBrac)

            else if hd.kind == TokLSqBrac then
                parseFunTreeArgs tl
                    |> Result.andThen (parseFunParExpected TokRSqBrac)

            else
                Err { location = hd.pos, msg = "Expected (, [, {" }

        _ ->
            Err { location = 0, msg = "Expected (, [, {" }


parseFunParExpected : TokenKind -> ( List FunTree, List Token ) -> Result ParseError ( List FunTree, List Token )
parseFunParExpected knd ( res, slc ) =
    case slc of
        hd2 :: rem ->
            if hd2.kind == knd then
                Ok ( res, rem )

            else
                Err { location = hd2.pos, msg = "Expected " ++ tokenKindToString knd ++ ", found " ++ tokenKindToString hd2.kind }

        [] ->
            Err { location = 0, msg = "Expected " ++ tokenKindToString knd ++ ", found EOF" }


parseFunTreeArgs : List Token -> Result ParseError ( List FunTree, List Token )
parseFunTreeArgs lst =
    case parseFunTree lst of
        Ok ( ex, toks ) ->
            case toks of
                hd :: tl ->
                    if hd.kind == TokComma then
                        parseFunTreeArgs tl |> Result.map (Tuple.mapFirst ((::) ex))

                    else
                        Ok ( [ ex ], toks )

                _ ->
                    Ok ( [ ex ], toks )

        Err e ->
            Err e


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



-- Check redefinitions and/or ambiguous definitions


checkPredicate : Domain -> Set String -> Expr -> Result String Expr
checkPredicate domain declared ex =
    case ex of
        One ->
            Ok One

        Zero ->
            Ok Zero

        Neg x ->
            checkPredicate domain declared x |> Result.map Neg

        And l r ->
            Result.map2 (\x y -> And x y) (checkPredicate domain declared l) (checkPredicate domain declared r)

        Or l r ->
            Result.map2 (\x y -> Or x y) (checkPredicate domain declared l) (checkPredicate domain declared r)

        Implies l r ->
            Result.map2 (\x y -> Implies x y) (checkPredicate domain declared l) (checkPredicate domain declared r)

        Iff l r ->
            Result.map2 (\x y -> Iff x y) (checkPredicate domain declared l) (checkPredicate domain declared r)

        Ident name ->
            if Set.member name domain.propositions then
                Ok ex

            else
                Err ("La proposición " ++ name ++ " no es conocida")

        Predicate name args ->
            -- Check arity
            case Dict.get name domain.predicates of
                Just v ->
                    if v == List.length args then
                        -- Go inside
                        List.map (checkFunTree domain declared) args
                            |> toResultAll
                            |> Result.map (Predicate name)

                    else
                        Err
                            ("El predicado "
                                ++ name
                                ++ " requiere "
                                ++ String.fromInt v
                                ++ " argumentos, pero solo se le han dado "
                                ++ String.fromInt (List.length args)
                            )

                Nothing ->
                    Err ("Predicado " ++ name ++ " desconocido")

        Forall name sub ->
            if Set.member name domain.domain then
                Err ("La variable " ++ name ++ " ya existe en el dominio, usa otro nombre")

            else if Set.member name declared then
                Err ("La variable " ++ name ++ " ya ha sido declarada en un nivel superior, no es posible reusarla dentro del mismo bloque")

            else
                checkPredicate domain (Set.insert name declared) sub |> Result.map (Forall name)

        Exists name sub ->
            if Set.member name domain.domain then
                Err ("La variable " ++ name ++ " ya existe en el dominio, usa otro nombre")

            else if Set.member name declared then
                Err ("La variable " ++ name ++ " ya ha sido declarada en un nivel superior, no es posible reusarla dentro del mismo bloque")

            else
                checkPredicate domain (Set.insert name declared) sub |> Result.map (Exists name)


checkFunTree : Domain -> Set String -> FunTree -> Result String FunTree
checkFunTree domain declared ftree =
    case ftree of
        Atom _ ->
            Ok ftree

        Apply name args ->
            case Dict.get name domain.functions of
                Just v ->
                    if v == List.length args then
                        List.map (checkFunTree domain declared) args |> toResultAll |> Result.map (Apply name)

                    else
                        Err
                            ("La función "
                                ++ name
                                ++ " recibe "
                                ++ String.fromInt v
                                ++ " argumentos, pero solo se le han dado "
                                ++ String.fromInt (List.length args)
                            )

                Nothing ->
                    Err ("La función " ++ name ++ " no está definida")


toResultAll : List (Result err ok) -> Result err (List ok)
toResultAll lst =
    case lst of
        (Ok v) :: tl ->
            toResultAll tl |> Result.map ((::) v)

        (Err v) :: _ ->
            Err v

        [] ->
            Ok []
