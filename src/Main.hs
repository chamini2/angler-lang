module Main where

import           Control.Lens
import           Control.Monad                 (unless, when)
import           Data.List                     (intercalate)
import           System.Console.GetOpt         (ArgOrder(..), getOpt)
import           System.Directory              (doesFileExist)
import           System.Exit                   (exitWith, ExitCode(..))
import           System.Environment            (getArgs)
import           System.IO                     (Handle, IOMode(..), hGetContents,
                                                openFile, stdin)

import           Language.Angler.AST
import           Language.Angler.Parser.Lexer  (evalLP, lexTokens)
import           Language.Angler.Parser.Parser (parseModule)
import           Language.Angler.MixfixParser  ()
import           Language.Angler.Options
import           Language.Angler.SrcLoc

main :: IO ()
main = do
        args <- getArgs

        -- Parse options, getting a list of option actions
        let (optActions, nonOptions, optErrors) = getOpt Permute optionDescrs args

        unless (null optErrors) (ioError (userError (concat optErrors)))

        options <- foldl (>>=) (return def) optActions

        (handle, filepath) <- case nonOptions of
                f : _ -> openFile f ReadMode >>= \h -> return (h, f)
                []    -> return (stdin, "<stdin>")

        print options

        readModule options handle filepath

readModule :: Options -> Handle -> FilePath -> IO ()
readModule options handle filepath = do
        putStrLn ("Checking module (" ++ filepath ++ ")")

        input <- hGetContents handle
        let evalLP' = evalLP input (SrcLoc filepath 1 1)

        when (view opt_tokens options) $ do
                putStrLn "\n\n***** lexer\n"
                case evalLP' lexTokens of
                        Right (ts,_) -> putStrLn (intercalate " " (map (show . view loc_insd) ts))
                        Left  _err   -> return ()

        ast <- case evalLP' parseModule of
                Right (ast,ws) -> mapM print ws >> return ast
                Left  err      -> printFailure err 1

        when (view opt_ast options) $ do
                putStrLn "\n\n***** parser\n"
                putStrLn (prettyShow ast)

        -- symbols <- readModule options modrelpath
        return ()
    where
        printFailure :: Show s => s -> Int -> IO a
        printFailure s code = print s >> exitWith (ExitFailure code)
