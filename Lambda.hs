{-# LANGUAGE ViewPatterns #-}

-- Simple implementation of the Lambda Calculus.

module Lambda where

-- The datatype of Lambda Calculus terms.
data Term 
    = Lam !Term 
    | App !Term !Term 
    | Var !Int 
    deriving (Show,Eq,Ord)

-- Folds over a term.
fold :: (t -> t) -> (t -> t -> t) -> (Int -> t) -> Term -> t
fold lam app var term = go term where
    go (Lam body)       = lam (go body)
    go (App left right) = app (go left) (go right)
    go (Var idx)        = var idx

-- Pretty prints a term.
pretty :: Term -> String
pretty (Var n)   = show n
pretty (Lam a)   = "λ"++pretty a
pretty (App a b) = "("++pretty a++" "++pretty b++")"

-- Reduces a strongly normalizing term to normal form.
-- Does not halt then the term isn't strongly normalizing. 
reduce :: Term -> Term
reduce (Lam a) = Lam (reduce a)
reduce (Var a) = Var a
reduce (App a b) = case reduce a of
    Lam body  -> reduce (subs (reduce b) True 0 (-1) body)
    otherwise -> App (reduce a) (reduce b) 
    where   
        subs t s d w (App a b) = App (subs t s d w a) (subs t s d w b)
        subs t s d w (Lam a)   = Lam (subs t s (d+1) w a) 
        subs t s d w (Var a)
            | s && a == d = subs (Var 0) False (-1) d t
            | otherwise   = Var (a + (if a > d then w else 0))

-- How many occurrences of the bound variable are on the term?
countBoundVar :: Term -> Int
countBoundVar term = fold lam app var term (-1) where
    lam body depth                   = body (depth+1)
    app left right depth             = left depth + right depth
    var index depth | depth == index = 1
    var index depth | otherwise      = 0
