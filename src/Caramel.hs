module Caramel where

import Control.Applicative ((<*>),(<$>))
import Control.Monad (msum,replicateM)
import Data.Char
import Data.List (intercalate,foldl1')
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe,fromJust)
import Text.ParserCombinators.ReadP
import Text.Printf
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Lambda as L

-- The main datatype of Caramel's syntax sugars.
data Caramel 
    = Lam [String] Caramel 
    | App [Caramel]
    | Var String 
    | Nat Int 
    | Lst [Caramel]
    | Tup [Caramel]
    | Chr Char
    | Str String
    | Wrd Int 
    | Adt [(String, [(String, Caramel)])]
    | Let [(String,[String],Caramel)] Caramel
    deriving Show

-- The usual fold over Caramel's constructors.
fold 
    :: ([String]->b->b) 
    -> ([b]->b) 
    -> (String->b) 
    -> (Int->b) 
    -> ([b]->b) 
    -> ([b]->b) 
    -> (Char->b) 
    -> (String->b) 
    -> (Int->b)
    -> ([(String,[(String,b)])]->b)
    -> ([(String,[String],b)]->b->b)
    -> Caramel 
    -> b
fold lam app var num lst tup chr str wrd adt leT = go where
    go (Lam vars body) = lam vars (go body) 
    go (App terms)     = app (map go terms)
    go (Var name)      = var name
    go (Nat n)         = num n
    go (Lst l)         = lst (map go l)
    go (Tup t)         = tup (map go t)
    go (Chr c)         = chr c
    go (Wrd c)         = wrd c
    go (Str s)         = str s
    go (Adt ctors)     = adt (map (\ (name,ctor) -> (name, map (\ (name,field) -> (name, go field)) ctor)) ctors)
    go (Let defs term) = leT (map (\ (name,vars,term) -> (name, vars, go term)) defs) (go term)

-- Prints a Caramel term with the Caramel syntax. The reverse of parsing.
pretty :: Caramel -> String
pretty term = fold lam app var num lst tup chr str wrd adt leT term [] where
    lam vars body = ("(" ++) . (unwords vars ++) . (" -> " ++) . body . (")" ++)
    app terms     = ("(" ++) . unwords' terms . (")" ++)
    var name      = (name ++)
    num n         = (show n ++)
    lst l         = ("[" ++) . inters' "," l . ("]" ++)
    tup l         = ("(" ++) . inters' "," l . (")" ++)
    chr c         = ("'" ++) . ([c] ++) . ("'" ++)
    str s         = ("\"" ++) . (s ++) . ("\"" ++)
    wrd w         = (("#" ++ show w) ++)
    adt d         = undefined
    leT d t       = ("{"++). (inters' "; " (map (\ (name,vars,term)->(unwords (name:vars)++).(" = "++).term) d)) . ("; "++) . t . ("}"++)
    unwords' list = foldr (\ h t s -> (s ++) . h . t " ") (const ([] ++)) list ""
    inters' sep list = foldr (\ h t s -> (s ++) . h . t sep) (const ([] ++)) list ""

-- Parses a Caramel source code into a Caramel term. The reverse of pretty-printing.
-- Does not deal with parse errors properly (TODO).
parse :: String -> Caramel
parse = fst . head . reverse . readP_to_S (term 0) . stripComments where
    -- The pre-processing just removes comments and empty lines
    stripComments
        = unlines 
        . (++ [""])             
        . filter (not . all isSpace) -- removes empty lines
        . map (head . splitOn "--")  -- removes comments
        . lines
    -- A term is one of the syntax sugars provided by the DSL, optimionally
    -- followed by local definitions.
    term d       = do
        parsedTerm <- lam d <++ leT d <++ tup d <++ choice [leT d,app d,str,chr,lst d,adt d] <++ wrd <++ num <++ var
        localDefs  <- many (string ('\n':replicate ((d+1)*4) ' ') >> def (d+1))
        return $ case localDefs of
            []   -> parsedTerm
            defs -> Let defs parsedTerm
    -- The sugars below implement Caramel's syntax as defined on the README.
    app d       = App <$> sepBetween "(" " " ")" (term d)
    var         = Var <$> choice [word, many1 (char '*')]
    num         = Nat <$> read <$> number
    lst d       = Lst <$> sepBetween "[" "," "]" (term d)
    tup d       = Tup <$> sepBetween "(" "," ")" (term d)
    chr         = Chr <$> (do { char '\''; c <- get; char '\''; return c})
    str         = Str <$> (do { char '"'; s <- manyTill get (char '"'); return s})
    wrd         = Wrd <$> read <$> (char '#' >> number)
    adt d       = Adt <$> sepBetween "#{" "|" "}" ctor where
        ctor    = paired (,) word space fields
        fields  = sepBetween "{" "," "}" field <++ return []
        field   = paired (,) word (char ':') (term d)
    lam d       = between (char '(') (char ')') (paired Lam vars (string "->") (term d)) where
        vars    = sepBy word (char ' ')
    def d       = paired pair names (char '=') (term d) where
        pair    = \ (name:vars) value -> (name, vars, value)
        names   = sepBy word (char ' ')
    leT d       = between (char '{' >> skipSpaces) (skipSpaces >> char '}') (paired Let defs (char ';') (term d)) where 
        defs    = sepBy (def d) (char ';' >> space)
    -- Some useful parse combinators. This code looks bad and could improve.
    number      = many1 (satisfy isDigit)
    letter      = satisfy isLetter
    word        = many1 (satisfy (\ c -> isAlphaNum c || elem c ("_.@:?#$!^&|*-+<>~=/"::String)))
    paired      = \ fn left sep right -> do { l <- left; space>>sep>>space; r <- right; return (fn l r) }
    sepBetween  = \ left sep right parse -> let 
        lSpace  = if head sep /= ' ' then space else return ()
        wrap    = between (string left) (string right)
        in wrap (sepBy parse (lSpace >> string sep >> space))
    space       = skipSpaces

-- Converts a Lambda Calculus term to a value of the Caramel DSL.
fromLambda :: L.Term -> Caramel
fromLambda term = L.fold lam app var term (M.empty :: M.Map Int String) 0 where

    -- Converts λ-calculus abstractions to Caramel.
    lam body scope depth 
        = strSugar
        . chrSugar
        . wrdSugar
        . lstSugar
        . tupSugar
        . natSugar
        . lamSugar 
        $ Lam [name] (body (M.insert depth name scope) (depth+1))
        where name = infiniteAlphabet !! depth

    -- Converts λ-calculus applications to Caramel.
    app left right scope depth 
        = appSugar 
        $ App [left scope depth, right scope depth]

    -- Converts λ-calculus variables to Caramel.
    var index scope depth 
        = Var (scope M.! (depth - index - 1))

    -- The lam sugar just removes consecutive lambdas, 
    -- i.e., (a -> (b -> c)) becomes (a b -> c)
    lamSugar :: Caramel -> Caramel
    lamSugar (Lam names (Lam names' body)) = Lam (names++names') (lamSugar body)
    lamSugar term                          = term

    -- The app sugar just removes redundant parens, 
    -- i.e., ((f x) y) becomes (f x y)
    appSugar :: Caramel -> Caramel
    appSugar (App (App args : args')) = appSugar (App (args ++ args'))
    appSugar term                     = term

    -- Church naturals to Nat, 
    -- i.e., (f x -> (f (f (f x)))) to 3
    natSugar :: Caramel -> Caramel
    natSugar term = maybe term id (getNat term) where
        getNat (Lam [fn,arg] vals) = Nat <$> go vals where
            go (App [Var f, p]) | f == fn   = (+ 1) <$> go p
            go (Var x)          | x == arg  = Just 0
            go _                | otherwise = Nothing
        getNat term = Just term

    -- Church lists to Lst, 
    -- i.e., (c n -> (c 1 (c 2 (c 3 n)))) to [1,2,3]
    lstSugar term = maybe term id (getLst term) where
        getLst (Lam [cons,nil] cells) = Lst <$> go cells where
            go (App [Var c, h, t]) 
                | c == cons 
                && not (freeVarInTerm cons h) 
                && not (freeVarInTerm nil h)
                = (h :) <$> go t
            go (Var n)             | n == nil  = Just []
            go _                   | otherwise = Nothing
        getLst term = Just term

    -- Church tuples to Tup, 
    -- i.e., (t -> (t 1 2 3)) to (1,2,3)
    tupSugar term@(Lam [tupVar] body@(App (Var t : xs)))
        | t == tupVar
        && not (any (freeVarInTerm tupVar) xs)
        = Tup xs
    tupSugar term = term
    
    -- Template function to create the Chr and Wrd sugar.
    -- bitVecSugar :: Caramel -> Caramel
    bitVecSugar size ctor term = maybe term id (getChr term) where
        getChr (Lam [fn,one,zero] (App (Var fnv : bits)))
            | fn == fnv 
            && length bits == size
            && all (\ (Var bit) -> bit == one || bit == zero) bits
            = Just . ctor . toEnum . toByte . map (\ (Var bit) -> bit) $ bits
            where toByte bits    = foldl makeByte (const 0) bits 1
                  makeByte t h b = (if h == one then 1 else 0) * b + (t (b*2))
        getChr term = Just term

    -- Church byte to Chr (ASCII-encoded char), 
    -- i.e., (f 1 0 -> (f 0 1 1 0 0 0 0 1)) to '\'a\''
    chrSugar :: Caramel -> Caramel
    chrSugar = bitVecSugar 8 Chr

    -- Church word to Wrd (Haskell's Word32)
    -- i.e., (f 1 0 -> (f 0 0 ...28 zeros... 0 1)) to '1''
    wrdSugar :: Caramel -> Caramel
    wrdSugar = bitVecSugar 32 Wrd

    -- Church string (list of Chrs) to Str, 
    -- i.e., ['a' 'b' 'c' 'd'] to "abcd"
    strSugar :: Caramel -> Caramel
    strSugar term = maybe term id (getStr term) where
        getStr (Lst chrs) | all isChr chrs = Just (Str (map (\ (Chr c) -> c) chrs)) where 
            isChr (Chr _)   = True
            isChr otherwise = False
        getStr term = Just term

    -- TODO: ADT fromLambda
    adt = undefined

    -- List of all strings consisting of upper/lowercase letters.
    infiniteAlphabet :: [String]
    infiniteAlphabet = do 
        x <- [1..]
        replicateM x (['a'..'z']++['A'..'Z'])
        
    -- Is given variable free in a term?
    freeVarInTerm :: String -> Caramel -> Bool
    freeVarInTerm varName = elem varName . freeVars

-- Converts a value of the Caramel DSL to a pure Lambda Calculus term.
toLambda :: Caramel -> L.Term
toLambda term = go term (M.empty :: M.Map String Int) 0 where
    go = fold lam app var num lst tup chr str wrd adt leT
    lam vars body = foldr cons body vars where
        cons var body scope depth = L.Lam (body (M.insert var depth scope) (depth+1))
    leT defs term = foldr cons term defs where 
        cons (name,vars,body) term scope depth = L.App (L.Lam (term (M.insert name depth scope) (depth+1))) (foldr cons' body vars scope depth)
        cons' var body scope depth             = L.Lam (body (M.insert var depth scope) (depth+1))
    app args scope depth = foldl1' snoc args scope depth where
        snoc left right scope depth = L.App (left scope depth) (right scope depth)
    var name scope depth = L.Var (depth - index - 1) where
        index = maybe (error ("undefined variable `"++name++"`.")) id (M.lookup name scope)
    num n scope depth = L.Lam (L.Lam (call n (L.App (L.Var 1)) (L.Var 0)))
    lst terms scope depth = L.Lam (L.Lam (foldr (\ h t -> L.App (L.App (L.Var 1) (h scope (depth+2))) t) (L.Var 0) terms))
    tup terms scope depth = L.Lam (foldl (\ t h -> L.App t (h scope (depth+1))) (L.Var 0) terms)
    chr c scope depth = L.Lam (L.Lam (L.Lam (foldl bits (L.Var 2) (printf "%08b" (fromEnum c) :: String))))
        where bits t h = L.App t (L.Var (fromEnum h - fromEnum '0'))
    str s scope depth = toLambda (Lst (map Chr s))
    wrd c scope depth = L.Lam (L.Lam (L.Lam (foldl bits (L.Var 2) (printf "%032b" (fromEnum c) :: String))))
        where bits t h = L.App t (L.Var (fromEnum h - fromEnum '0'))
    adt ctors scope depth  = L.Lam (L.App (L.Var 0) (list (map ctor ctors))) where
        ctor (name,ctor)   = pair (toLambda (Str name)) (applyConstToBoundVar (L.Lam (list (map field ctor))))
        field (name,field) = pair (toLambda (Str name)) (L.App (field (M.insert "*" (depth+4) scope) (depth+8)) (L.Var 7))
        list term          = L.Lam (L.Lam (foldr (\ h t -> L.App (L.App (L.Var 1) h) t) (L.Var 0) term))
        pair a b           = L.Lam (L.App (L.App (L.Var 0) a) b)
        applyConstToBoundVar term = L.fold lam app var term (-1) where
            lam body depth                   = L.Lam (body (depth+1))
            app left right depth             = L.App (left depth) (right depth)
            var index depth | index == depth = L.Lam (L.Var (index+1))
            var index depth | otherwise      = L.Var index
    call n f x = go n x where
        go 0 x = x
        go k x = go (k-1) (f x)

-- Returns a list of the free variables in a Caramel term.
freeVars :: Caramel -> [String]
freeVars term = fold lam app var nat lst tup chr str wrd adt leT term S.empty where 
    lam vars body boundVars = body (foldr S.insert boundVars vars)
    app terms boundVars     = concatMap ($ boundVars) terms
    var varName boundVars   = if S.member varName boundVars then [] else [varName]
    nat _ boundVars         = []
    lst terms boundVars     = concatMap ($ boundVars) terms
    tup terms boundVars     = concatMap ($ boundVars) terms
    chr _ boundVars         = []
    str _ boundVars         = []
    wrd _ boundVars         = []
    adt ctors boundVars     = concatMap (concatMap (($ boundVars) . snd) . snd) ctors
    leT defs term boundVars 
        = term (foldr S.insert boundVars (map (\ (name,_,_) -> name) defs))
        ++ concatMap (\ (_,vars,body) -> body (foldr S.insert boundVars vars)) defs

-- Sorts let expressions so that a term that depends on the other always come before.
-- Also adds an extra bound variable for recursive terms, in order to enable further use
-- with fixed-point combinators and similars, i.e., 
-- `sum n     = (is_zero? n 0 (add n (sum (pred n 1))))` becomes 
-- `sum sum n = (is_zero? n 0 (add n (sum (pred n 1))))`
-- So it can be used as `(Y sum 3)` (`Y` being the Y-combinator).
sortRecursiveLets :: Caramel -> Caramel
sortRecursiveLets = fold Lam App Var Nat Lst Tup Chr Str Wrd Adt leT where
    leT defs term = Let (sortTopologically (map node defs)) term where 
        names = S.fromList (map (\ (name,_,_) -> name) defs)
        node def@(name, vars, body) = (name, dependencies, defWithFixedPoint) where 
            dependencies      = filter (/= name) . filter (flip S.member names) $ freeVars'
            defWithFixedPoint = (name, if elem name freeVars' then name:vars else vars, body)
            freeVars'         = freeVars (Lam vars body)
    -- Naive implementation of a topological sort, O(N^2). Potential bottleneck. TODO: improve.
    sortTopologically :: [(String, [String], a)] -> [a]
    sortTopologically graph = go graph (S.empty :: S.Set String) [] where
        go :: [(String, [String], a)] -> S.Set String -> [(String, [String], a)] -> [a]
        go [] defined []   = []
        go [] defined rest = go rest defined []
        go ((node@(id, deps, val)) : nodes) defined rest
            | all (flip S.member defined) deps = val : go nodes (S.insert id defined) rest
            | otherwise                        = go nodes defined (node:rest)

-- Evaluates a Caramel term by converting it to the Lambda Calculus, reducing and reading back.
reduce :: Caramel -> Caramel
reduce = fromLambda . L.reduce . toLambda
