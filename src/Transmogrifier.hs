-- Compiles a λ-calculus program to anything.

module Transmogrifier where

import Lambda
import Util

transmogrify :: (String -> String -> String) -> (String -> String -> String) -> Term -> String
transmogrify lam app = foldScoped (lam.(infiniteAlphabet!!)) app (\ index -> infiniteAlphabet!!index)

toJavaScript :: Term -> String
toJavaScript = transmogrify lam app where
    lam var body    = "(function("++var++"){return "++body++"})"
    app left right  = left ++ "(" ++ right ++ ")"

toPython :: Term -> String
toPython = transmogrify lam app where
    lam var body    = "(lambda "++var++": "++body++")"
    app left right  = left++"("++right++")"

toScheme :: Term -> String
toScheme = transmogrify lam app where
    lam var body   = "(lambda("++var++")"++body++")"
    app left right = "("++left++" "++right++")"

toHaskell :: Term -> String
toHaskell term = "(let (#) = unsafeCoerce in " ++ transmogrify lam app term ++")" where
    lam var body   = "(\\"++var++"->"++body++")"
    app left right = "("++left++"#"++right++")"

toLua :: Term -> String
toLua = transmogrify lam app where
    lam var body    = "(function ("++var++") return "++body++" end)"
    app left right  = left ++ "(" ++ right ++ ")"

toRuby :: Term -> String
toRuby = transmogrify lam app where
    lam var body    = "(->("++var++"){"++body++"})"
    app left right  = left ++ ".(" ++ right ++ ")"

toOptlam :: Term -> String
toOptlam = fold lam app var where
    lam body        = "L("++body++")"
    app left right  = "A("++left++","++right++")"
    var index       = "V("++show index++")"

toBinaryLambdaCalculus :: Term -> String
toBinaryLambdaCalculus = fold lam app var where
    lam body       = "00" ++ body
    app left right = "01" ++ left ++ right
    var index      = replicate (index+1) '1' ++ "0"

toAst :: Term -> String
toAst = fold lam app var where
    lam body       = "Lam(" ++ body ++ ")"
    app left right = "App(" ++ left ++ "," ++ right ++ ")"
    var index      = "Var(" ++ show index ++ ")"
