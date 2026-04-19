module Match2 exposing (emptyClues, match, matchAll, matchAllTwice, replace)

import Dict exposing (Dict)
import Expr.Types exposing (Expr(..), FunTree(..))
import Utils exposing (toResultAll)



-- Reimplementation of the inferring algorithm


type alias Clues =
    { variables : Dict String String
    , propositions : Dict String Expr
    , predicates : Dict String Expr
    , values : Dict String FunTree
    , predArgNames : Dict String (List String)
    }


emptyClues : Clues
emptyClues =
    { variables = Dict.empty, propositions = Dict.empty, predicates = Dict.empty, values = Dict.empty, predArgNames = Dict.empty }


matchAllTwice : List Expr -> List Expr -> Clues -> Result () Clues
matchAllTwice exs pats clues =
    matchAll exs pats clues |> Result.andThen (matchAll exs pats)


matchAll : List Expr -> List Expr -> Clues -> Result () Clues
matchAll exs pats clues =
    case ( exs, pats ) of
        ( hd1 :: tl1, hd2 :: tl2 ) ->
            match hd1 hd2 clues |> Result.andThen (matchAll tl1 tl2)

        ( [], [] ) ->
            Ok clues

        _ ->
            Err ()



-- Match a given expression against a given pattern


match : Expr -> Expr -> Clues -> Result () Clues
match ex pat clues =
    case ( ex, pat ) of
        ( One, One ) ->
            Ok clues

        ( Zero, Zero ) ->
            Ok clues

        ( prop, Ident name ) ->
            -- Check if already found
            case Dict.get name clues.propositions of
                Just v ->
                    if prop == v then
                        Ok clues

                    else
                        -- Do not match
                        Err ()

                Nothing ->
                    Ok { clues | propositions = Dict.insert name prop clues.propositions }

        ( Neg sub1, Neg sub2 ) ->
            match sub1 sub2 clues

        ( And el er, And pl pr ) ->
            match el pl clues |> Result.andThen (match er pr)

        ( Or el er, Or pl pr ) ->
            match el pl clues |> Result.andThen (match er pr)

        ( Implies el er, Implies pl pr ) ->
            match el pl clues |> Result.andThen (match er pr)

        ( Iff el er, Iff pl pr ) ->
            match el pl clues |> Result.andThen (match er pr)

        ( Forall name1 sub1, Forall name2 sub2 ) ->
            { clues | variables = Dict.insert name2 name1 clues.variables }
                |> match sub1 sub2

        ( Exists name1 sub1, Exists name2 sub2 ) ->
            { clues | variables = Dict.insert name2 name1 clues.variables }
                |> match sub1 sub2

        ( left, Predicate name args ) ->
            -- Dont worry with composition (by now, must revisit later)
            -- All the args must be free variables in order to recognize this
            if
                List.all
                    (\v ->
                        case v of
                            Atom atomName ->
                                Dict.member atomName clues.variables

                            _ ->
                                False
                    )
                    args
            then
                -- All the args are just variables not functions
                case Dict.get name clues.predicates of
                    Just saved ->
                        -- They must be equal
                        if saved == left then
                            Ok clues

                        else
                            Err ()

                    Nothing ->
                        -- Save
                        Ok
                            { clues
                                | predicates = Dict.insert name left clues.predicates
                                , predArgNames = Dict.insert name (List.filterMap nameOfNode args) clues.predArgNames
                            }

            else
                case ( Dict.get name clues.predArgNames, Dict.get name clues.predicates ) of
                    ( Just names, Just original ) ->
                        let
                            whatToReplace =
                                List.map2 Tuple.pair names args
                                    |> Dict.fromList
                        in
                        (replaceInOriginalE whatToReplace original
                            |> extractCluesPredicate left
                        )
                            clues

                    _ ->
                        -- Nothing to do
                        Ok clues

        _ ->
            Err ()


replaceInOriginalE : Dict String FunTree -> Expr -> Expr
replaceInOriginalE dik ex =
    case ex of
        One ->
            One

        Zero ->
            Zero

        Ident a ->
            Ident a

        Neg e ->
            replaceInOriginalE dik e |> Neg

        And l r ->
            And (replaceInOriginalE dik l) (replaceInOriginalE dik r)

        Or l r ->
            Or (replaceInOriginalE dik l) (replaceInOriginalE dik r)

        Implies l r ->
            Implies (replaceInOriginalE dik l) (replaceInOriginalE dik r)

        Iff l r ->
            Iff (replaceInOriginalE dik l) (replaceInOriginalE dik r)

        Forall x s ->
            Forall x (replaceInOriginalE dik s)

        Exists x s ->
            Exists x (replaceInOriginalE dik s)

        Predicate n a ->
            List.map (replaceInOriginalF dik) a |> Predicate n


replaceInOriginalF : Dict String FunTree -> FunTree -> FunTree
replaceInOriginalF dik ft =
    case ft of
        Atom name ->
            case Dict.get name dik of
                Just v ->
                    v

                Nothing ->
                    Atom name

        Apply n a ->
            List.map (replaceInOriginalF dik) a |> Apply n


nameOfNode : FunTree -> Maybe String
nameOfNode fnt =
    case fnt of
        Atom name ->
            Just name

        _ ->
            Nothing


extractCluesPredicate : Expr -> Expr -> Clues -> Result () Clues
extractCluesPredicate ex pat clues =
    case ( ex, pat ) of
        ( One, One ) ->
            Ok clues

        ( Zero, Zero ) ->
            Ok clues

        ( Ident n1, Ident n2 ) ->
            if n1 == n2 then
                Ok clues

            else
                Err ()

        ( Neg sub1, Neg sub2 ) ->
            extractCluesPredicate sub1 sub2 clues

        ( And l1 l2, And r1 r2 ) ->
            extractCluesPredicate l1 r1 clues |> Result.andThen (extractCluesPredicate l2 r2)

        ( Or l1 l2, Or r1 r2 ) ->
            extractCluesPredicate l1 r1 clues |> Result.andThen (extractCluesPredicate l2 r2)

        ( Implies l1 l2, Implies r1 r2 ) ->
            extractCluesPredicate l1 r1 clues |> Result.andThen (extractCluesPredicate l2 r2)

        ( Iff l1 l2, Iff r1 r2 ) ->
            extractCluesPredicate l1 r1 clues |> Result.andThen (extractCluesPredicate l2 r2)

        ( Forall name1 sub1, Forall name2 sub2 ) ->
            if name1 /= name2 then
                Err ()

            else
                extractCluesPredicate sub1 sub2 clues

        ( Exists name1 sub1, Exists name2 sub2 ) ->
            if name1 /= name2 then
                Err ()

            else
                extractCluesPredicate sub1 sub2 clues

        ( Predicate name1 args1, Predicate name2 args2 ) ->
            if name1 == name2 then
                extractCluesFunTreeArgs args1 args2 clues

            else
                Err ()

        _ ->
            Err ()


extractCluesFunTree : FunTree -> FunTree -> Clues -> Result () Clues
extractCluesFunTree ex pat clues =
    case ( ex, pat ) of
        ( left, Atom name ) ->
            case Dict.get name clues.values of
                Just v ->
                    if left == v then
                        Ok clues

                    else
                        Err ()

                Nothing ->
                    Ok { clues | values = Dict.insert name left clues.values }

        ( Apply name1 args1, Apply name2 args2 ) ->
            if name1 == name2 then
                extractCluesFunTreeArgs args1 args2 clues

            else
                Err ()

        _ ->
            Err ()


extractCluesFunTreeArgs : List FunTree -> List FunTree -> Clues -> Result () Clues
extractCluesFunTreeArgs exs pats clues =
    case ( exs, pats ) of
        ( hd1 :: tl1, hd2 :: tl2 ) ->
            extractCluesFunTree hd1 hd2 clues |> Result.andThen (extractCluesFunTreeArgs tl1 tl2)

        ( [], [] ) ->
            Ok clues

        _ ->
            Err ()



-- Replace the clues in the given expression


replace : Expr -> Clues -> Result () Expr
replace ex clues =
    case ex of
        One ->
            Ok One

        Zero ->
            Ok Zero

        Ident name ->
            case Dict.get name clues.propositions of
                Just v ->
                    Ok v

                Nothing ->
                    Err ()

        Neg sub ->
            replace sub clues |> Result.map Neg

        And l r ->
            Result.map2 And (replace l clues) (replace r clues)

        Or l r ->
            Result.map2 Or (replace l clues) (replace r clues)

        Implies l r ->
            Result.map2 Implies (replace l clues) (replace r clues)

        Iff l r ->
            Result.map2 Iff (replace l clues) (replace r clues)

        Forall name sub ->
            case Dict.get name clues.variables of
                Just newname ->
                    replace sub clues |> Result.map (Forall newname)

                Nothing ->
                    Err ()

        Exists name sub ->
            case Dict.get name clues.variables of
                Just newname ->
                    replace sub clues |> Result.map (Exists newname)

                Nothing ->
                    Err ()

        Predicate name args ->
            case Dict.get name clues.predicates of
                Just origpred ->
                    replaceButInFunTree origpred (makePredRepl name clues args) clues

                Nothing ->
                    Err ()


makePredRepl : String -> Clues -> List FunTree -> Dict String FunTree
makePredRepl name clues args =
    case Dict.get name clues.predArgNames of
        Just v ->
            List.map2 Tuple.pair v args |> Dict.fromList

        Nothing ->
            Dict.empty


replaceButInFunTree : Expr -> Dict String FunTree -> Clues -> Result () Expr
replaceButInFunTree ex repl clues =
    case ex of
        One ->
            Ok One

        Zero ->
            Ok Zero

        Ident _ ->
            Ok ex

        Neg sub ->
            replaceButInFunTree sub repl clues |> Result.map Neg

        And l r ->
            Result.map2 And (replaceButInFunTree l repl clues) (replaceButInFunTree r repl clues)

        Or l r ->
            Result.map2 Or (replaceButInFunTree l repl clues) (replaceButInFunTree r repl clues)

        Implies l r ->
            Result.map2 Implies (replaceButInFunTree l repl clues) (replaceButInFunTree r repl clues)

        Iff l r ->
            Result.map2 Iff (replaceButInFunTree l repl clues) (replaceButInFunTree r repl clues)

        Forall name sub ->
            replaceButInFunTree sub repl clues |> Result.map (Forall name)

        Exists name sub ->
            replaceButInFunTree sub repl clues |> Result.map (Exists name)

        Predicate name args ->
            -- Replace things in the args
            List.map (replaceAllInFT repl clues) args |> Predicate name |> Ok


replaceAllInFT : Dict String FunTree -> Clues -> FunTree -> FunTree
replaceAllInFT dik clues ft =
    case ft of
        Atom name ->
            case ( Dict.get name dik, Dict.get name clues.variables ) of
                ( Just val, _ ) ->
                    replaceAllInFTEvenMore clues val

                ( _, Just newname ) ->
                    Atom newname

                _ ->
                    ft

        Apply name args ->
            List.map (replaceAllInFT dik clues) args |> Apply name


replaceAllInFTEvenMore : Clues -> FunTree -> FunTree
replaceAllInFTEvenMore clues ft =
    case ft of
        Atom name ->
            case Dict.get name clues.values of
                Just newval ->
                    newval

                Nothing ->
                    Atom name

        Apply name args ->
            List.map (replaceAllInFTEvenMore clues) args |> Apply name
