import Caramel
import Control.Applicative
import Control.Monad (when)
import Data.List
import Data.List.Split (splitOn)
import System.Directory
import System.Environment
import System.Exit
import qualified Lambda as L

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
    let format = case symbolFormat of
            "lam"     -> L.pretty . toLambda . reduce
            "lam?"    -> L.pretty . toLambda
            otherwise -> pretty . reduce
    putStrLn (format result)
