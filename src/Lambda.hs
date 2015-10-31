{-# LANGUAGE ViewPatterns, GADTs, FlexibleInstances, ScopedTypeVariables #-}

-- Simple implementation of the Lambda Calculus.

module Lambda where

import qualified Data.IntMap as IM

-- The datatype of Lambda Calculus terms.
data Term 
    = Lam !Term 
    | App !Term !Term 
    | Var !Int 
    deriving (Show,Eq,Ord)

-- HOAS term for normalization.
data HOAS a 
    = HPure a 
    | HLam (HOAS a -> HOAS a) 
    | HApp (HOAS a) (HOAS a)

-- Folds over a term.
fold :: (t -> t) -> (t -> t -> t) -> (Int -> t) -> Term -> t
fold lam app var term = go term where
    go (Lam body)       = lam (go body)
    go (App left right) = app (go left) (go right)
    go (Var idx)        = var idx

-- Folds over a term with scoped/named variables (using Int names).
foldScoped :: (Int -> t -> t) -> (t -> t -> t) -> (Int -> t) -> Term -> t
foldScoped lam app var term = fold lam' app' var' term 0 where
    lam' body depth       = lam depth (body (depth+1))
    app' left right depth = app (left depth) (right depth)
    var' idx depth        = var (depth-1-idx)

-- Pretty prints a term.
pretty :: Term -> String
pretty (Var n)   = show n
pretty (Lam a)   = "λ"++pretty a
pretty (App a b) = "("++pretty a++" "++pretty b++")"

-- Reduces a strongly normalizing term to normal form.
-- Does not halt then the term isn't strongly normalizing. 
reduceNaive :: Term -> Term
reduceNaive (Lam a) = Lam (reduceNaive a)
reduceNaive (Var a) = Var a
reduceNaive (App a b) = case reduceNaive a of
    Lam body  -> reduceNaive (subs (reduceNaive b) True 0 (-1) body)
    otherwise -> App (reduceNaive a) (reduceNaive b) 
    where   
        subs t s d w (App a b) = App (subs t s d w a) (subs t s d w b)
        subs t s d w (Lam a)   = Lam (subs t s (d+1) w a) 
        subs t s d w (Var a)
            | s && a == d = subs (Var 0) False (-1) d t
            | otherwise   = Var (a + (if a > d then w else 0))

-- Reduces a term to normal form through the host language.
reduce :: Term -> Term
reduce = reduceHOAS . toHOAS where

    -- Converts a term to a HOAS term.
    toHOAS :: forall a . Term -> HOAS a
    toHOAS term = go term IM.empty 0 where
        go :: Term -> IM.IntMap (HOAS a) -> Int -> HOAS a
        go (App a b) vars depth = applyHOAS (go a vars depth) (go b vars depth)
        go (Lam a) vars depth   = HLam $ \ var -> go a (IM.insert depth var vars) (depth+1)
        go (Var i) vars depth   = vars IM.! (depth-1-i)

    -- Applies a HOAS term to another.
    applyHOAS :: HOAS a -> HOAS a -> HOAS a
    applyHOAS (HLam a) b = a b
    applyHOAS a        b = HApp a b

    -- Reduces a HOAS term to the corresponding Term in normal form.
    reduceHOAS :: HOAS Int -> Term
    reduceHOAS = go 0 where
        go depth (HPure i)  = Var (depth-1-i)
        go depth (HLam a)   = Lam (go (depth+1) (a (HPure depth)))
        go depth (HApp a b) = App (go depth a) (go depth b)
