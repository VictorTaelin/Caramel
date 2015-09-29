{-# LANGUAGE BangPatterns #-}

module Util where

import Control.Monad (replicateM)

-- Infinite list of all strings consisting of upper/lowercase letters.
infiniteAlphabet :: [String]
infiniteAlphabet = do 
    x <- [1..]
    replicateM x (['a'..'z']++['A'..'Z'])

-- Calls a function `n` times.
call :: (Num a, Eq a) => a -> (t -> t) -> t -> t
call n f x = go n x where
    go !0 !x = x
    go !k !x = go (k-1) (f x)
{-# INLINE call #-}
