module Expr.Parser exposing (parse)

import Combinators exposing (Addr(..), Recognizer, accept, chain, chain3, choice, choice3, end, expect, lazy, map, maybe, maybeDefault)
import Dict
import Expr.Tokenizer exposing (..)
import Expr.Types exposing (..)
import List exposing (concat)
import Set exposing (Set)
import Utils exposing (toResultAll)


{-| Parse an expression
-}
parse : Domain -> String -> Result ParseError Expr
parse domain s =
    parseNoCheckPred s |> Result.andThen (checkMapping domain)


checkMapping : Domain -> Expr -> Result ParseError Expr
checkMapping domain ex =
    checkPredicate domain Set.empty ex |> Result.mapError (\msg -> { location = Expr.Types.End, msg = msg })


parseNoCheckPred : String -> Result ParseError Expr
parseNoCheckPred s =
    let
        tokens =
            tokenize 1 (String.toList s)
    in
    case goal .pos tokens of
        Ok ( e, _ ) ->
            Ok e

        Err err ->
            Err { location = addr2loc err.addr, msg = Set.fromList err.expected |> Set.toList |> String.join ", " |> (++) "Se esperaba: " }


addr2loc : Addr -> Loc
addr2loc a =
    case a of
        Combinators.Pos p ->
            Expr.Types.Pos p

        Combinators.End ->
            Expr.Types.End



-- The (pseudo) BNF of the expressions
--
-- goal   : ^ expr $
--
-- expr   : iff
--
-- iff    : impl '<->' iff
-- iff    : impl
--
-- impl   : or '->' impl
-- impl   : or
--
-- or     : and '|' or
-- or     : and
--
-- and    : neg '&' and
-- and    : neg
--
-- neg    : '-' neg
-- neg    : qnt
--
-- qnt    : '∀' '(' NAME ')' neg
-- qnt    : '∀' '{' NAME '}' neg
-- qnt    : '∀' '[' NAME ']' neg
-- qnt    : '∃' '(' NAME ')' neg
-- qnt    : '∃' '{' NAME '}' neg
-- qnt    : '∃' '[' NAME ']' neg
-- qnt    : atom
--
-- atom   : '1'
-- atom   : '0'
-- atom   : NAME
-- atom   : NAME '(' funtree ')'
-- atom   : NAME '{' funtree '}'
-- atom   : NAME '[' funtree ']'
-- atom   : funtree = funtree
-- atom   : '(' expr ')'
-- atom   : '{' expr '}'
-- atom   : '[' expr ']'
--
-- funtree : NAME
-- funtree : NAME '(' funtree_args ')'
--
-- funtree_args : funtree
-- funtree_args : funtree ',' funtree_args
--


goal : Recognizer Expr Token
goal =
    chain (\v _ -> v) expr (end |> expect "FIN")


expr : Recognizer Expr Token
expr =
    iff


iff : Recognizer Expr Token
iff =
    chain (applyIfNecessary Iff)
        impl
        (maybe <| chain (\_ v -> v) (litToken TokIff) (lazy (\_ -> iff)))


impl : Recognizer Expr Token
impl =
    chain (applyIfNecessary Implies)
        or
        (maybe <| chain (\_ v -> v) (litToken TokImplies) (lazy (\_ -> impl)))


or : Recognizer Expr Token
or =
    chain (applyIfNecessary Or)
        and
        (maybe <| chain (\_ v -> v) (litToken TokOr) (lazy (\_ -> or)))


and : Recognizer Expr Token
and =
    chain (applyIfNecessary And)
        neg
        (maybe <| chain (\_ v -> v) (litToken TokAnd) (lazy (\_ -> and)))


applyIfNecessary : (a -> a -> a) -> a -> Maybe a -> a
applyIfNecessary fn x mx =
    case mx of
        Just v ->
            fn x v

        Nothing ->
            x


neg : Recognizer Expr Token
neg =
    choice
        qnt
        (chain (\_ v -> v) (litToken TokNot) (lazy (\_ -> neg)) |> map Neg)


qnt : Recognizer Expr Token
qnt =
    choice3
        (chain3 (\_ -> Forall) (litToken TokForall) qntArg (lazy (\_ -> qnt)))
        (chain3 (\_ -> Exists) (litToken TokExists) qntArg (lazy (\_ -> qnt)))
        atom


qntArg : Recognizer String Token
qntArg =
    choice3
        (chain3 (\_ v _ -> v) (litToken TokLPar) tokName (litToken TokRPar))
        (chain3 (\_ v _ -> v) (litToken TokLBrac) tokName (litToken TokRBrac))
        (chain3 (\_ v _ -> v) (litToken TokLSqBrac) tokName (litToken TokRSqBrac))


atom : Recognizer Expr Token
atom =
    --Looks like lisp LMAO
    choice
        (choice
            (litToken (TokNum "1") |> map (\_ -> One))
            (litToken (TokNum "0") |> map (\_ -> Zero))
        )
        (choice3
            (tokName |> map Ident)
            atomPredic
            atomSubExpr
        )
        |> map replaceOneOrZero


replaceOneOrZero : Expr -> Expr
replaceOneOrZero ex =
    if ex == Ident "1" then
        One

    else if ex == Ident "0" then
        Zero

    else
        ex


atomPredic : Recognizer Expr Token
atomPredic =
    choice
        (chain Predicate tokName funTreeArgsPar)
        atomPredicEq


atomPredicEq : Recognizer Expr Token
atomPredicEq =
    chain3 (\l _ r -> Predicate "=" [ l, r ]) funTree (litToken TokEq) funTree


atomSubExpr : Recognizer Expr Token
atomSubExpr =
    choice3
        (chain3 (\_ v _ -> v) (litToken TokLPar) (lazy (\_ -> expr)) (litToken TokRPar))
        (chain3 (\_ v _ -> v) (litToken TokLBrac) (lazy (\_ -> expr)) (litToken TokRBrac))
        (chain3 (\_ v _ -> v) (litToken TokLSqBrac) (lazy (\_ -> expr)) (litToken TokRSqBrac))


funTree : Recognizer FunTree Token
funTree =
    choice
        (chain Apply
            tokName
            funTreeArgsPar
        )
        (tokName |> map Atom)


funTreeArgsPar : Recognizer (List FunTree) Token
funTreeArgsPar =
    choice3
        (chain3 (\_ v _ -> v) (litToken TokLPar) funTreeArgs (litToken TokRPar))
        (chain3 (\_ v _ -> v) (litToken TokLBrac) funTreeArgs (litToken TokRBrac))
        (chain3 (\_ v _ -> v) (litToken TokLSqBrac) funTreeArgs (litToken TokRSqBrac))


funTreeArgs : Recognizer (List FunTree) Token
funTreeArgs =
    chain (::)
        (lazy (\_ -> funTree))
        (chain (\_ v -> v) (litToken TokComma) (lazy (\_ -> funTreeArgs)) |> maybeDefault [])



-- Utilities


tokName : Recognizer String Token
tokName =
    accept
        (\t ->
            case t.kind of
                TokIdent v ->
                    Just v

                TokNum v ->
                    Just v

                _ ->
                    Nothing
        )
        |> expect "NOMBRE"


litToken : TokenKind -> Recognizer () Token
litToken tk =
    accept
        (\t ->
            if t.kind == tk then
                Just ()

            else
                Nothing
        )
        |> expect (tokenKindToString tk)



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
            if isVariable name |> not then
                Err ("Las variables deben empezar en minúscula (" ++ name ++ ")")

            else if Set.member name domain.domain then
                Err ("La variable " ++ name ++ " ya existe en el dominio, usa otro nombre")

            else if Set.member name declared then
                Err ("La variable " ++ name ++ " ya ha sido declarada en un nivel superior, no es posible reusarla dentro del mismo bloque")

            else
                checkPredicate domain (Set.insert name declared) sub |> Result.map (Forall name)

        Exists name sub ->
            if isVariable name |> not then
                Err ("Las variables deben empezar en minúscula (" ++ name ++ ")")

            else if Set.member name domain.domain then
                Err ("La variable " ++ name ++ " ya existe en el dominio, usa otro nombre")

            else if Set.member name declared then
                Err ("La variable " ++ name ++ " ya ha sido declarada en un nivel superior, no es posible reusarla dentro del mismo bloque")

            else
                checkPredicate domain (Set.insert name declared) sub |> Result.map (Exists name)


checkFunTree : Domain -> Set String -> FunTree -> Result String FunTree
checkFunTree domain declared ftree =
    case ftree of
        Atom name ->
            if String.toList name |> List.head |> Maybe.map Char.isLower |> Maybe.withDefault False then
                -- Es variable
                Ok ftree

            else
            -- Es valor
            if
                Set.member name domain.domain
            then
                Ok ftree

            else
                Err ("Valor " ++ name ++ " desconocido")

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
