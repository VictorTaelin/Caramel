import Caramel
import Control.Applicative
import Control.Monad (when)
import Data.List
import Data.List.Split (splitOn)
import System.Directory
import System.Environment
import System.Exit
import qualified Lambda as L
import qualified Transmogrifier as T

main = do
    dir       <- getCurrentDirectory
    fileNames <- fmap (filter (".mel" `isSuffixOf`)) (getDirectoryContents dir)
    contents  <- fmap concat (mapM readFile fileNames)
    args      <- getArgs
    when (null args) $ do
        putStrLn $ unlines [
            "Usage:",
            "    mel term_name      # Evaluates term_name and prints the result on the Caramel syntax.",
            "    mel term_name.lam  # Evaluates term_name and prints the result on the Lambda Calculus.",
            "You must be in a directory with a `.mel` file containing the `term_name` definition.",
            "Visit http://github.com/maiavictor/caramel for more info."]
        exitFailure
    let symbolFile = head args
    let (symbolName:symbolFormat:_) = splitOn "." symbolFile ++ ["mel"]
    let source = symbolName ++ "\n" ++ (unlines . map ("    " ++) . lines $ contents)
    let result = sortRecursiveLets $ parse source
    let reduce = if last symbolFormat == '!' then L.reduceNaive else L.reduce
    let format = case filter (/= '!') symbolFormat of
            "lam"     -> L.pretty                 . reduce     . toLambda
            "lam?"    -> L.pretty                              . toLambda
            "js"      -> T.toJavaScript           . reduce     . toLambda
            "scm"     -> T.toScheme               . reduce     . toLambda
            "lua"     -> T.toLua                  . reduce     . toLambda
            "hs"      -> T.toHaskell              . reduce     . toLambda
            "py"      -> T.toPython               . reduce     . toLambda
            "rb"      -> T.toRuby                 . reduce     . toLambda
            "opt"     -> T.toOptlam               . reduce     . toLambda
            "blc"     -> T.toBinaryLambdaCalculus . reduce     . toLambda
            otherwise -> pretty      . fromLambda . reduce     . toLambda
    putStrLn (format result)
