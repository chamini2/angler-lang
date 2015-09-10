module Main where

import           Language.Angler.AST           (prettyShow)
import           Language.Angler.Parser.Lexer  (evalLP)
import           Language.Angler.Parser.Parser (parseModule)
import           Language.Angler.SrcLoc        (SrcLoc(..))

import           Control.Lens
import           Control.Monad                 (unless, when)
import           Data.Default                  (Default(..))
import           System.Console.GetOpt         (ArgOrder(..), getOpt)
import           System.Environment            (getArgs)
import           System.IO                     (Handle, IOMode(..), hGetContents,
                                                openFile, stdin)

-- import           Language.Angler.Error
import           Language.Angler.SrcLoc
import           Language.Angler.Options
import           Language.Angler.Parser.Lexer  (lexTokens)
import           Language.Angler.Parser.Token

import           Data.List                     (intercalate)

main :: IO ()
main = do
        args <- getArgs

        -- Parse options, getting a list of option actions
        let (optActions, nonOptions, optErrors) = getOpt Permute optionDescrs args

        unless (null optErrors) $ ioError (userError (concat optErrors))

        options <- foldl (>>=) (return def) optActions

        (handle, filepath) <- case nonOptions of
                f : _ -> openFile f ReadMode >>= \h -> return (h, f)
                []    -> return (stdin, "<stdin>")

        print options

        angler options handle filepath

angler :: Options -> Handle -> FilePath -> IO ()
angler options handle filepath = do
        putStrLn $ "Checking module (" ++ filepath ++ ")"

        input <- hGetContents handle
        let evalLP' = evalLP input (SrcLoc filepath 1 1)

        when (view opt_tokens options) $ do
                putStrLn "\n\n***** lexer\n"
                case evalLP' lexTokens of
                        Right ltks -> putStrLn . intercalate " " $ map (showNL . view loc_insd) ltks
                            where
                                showNL tk = (case tk of
                                       TkVSemicolon -> (++ "\n") . show
                                       TkVLCurly    -> (++ "\n") . show
                                       TkVRCurly    -> ("\n" ++) . show
                                       _            -> show) tk

                        _ -> return ()

        when (view opt_ast options) $ do
                putStrLn "\n\n***** parser\n"
                case evalLP' parseModule of
                        Right lmod -> putStrLn (prettyShow lmod)
                        Left  err  -> print err

        -- symbols <- readModule options modrelpath
    where


-- readModule :: Options -> FilePath -> IO a
-- readModule options modrelpath = do
--         let paths = view opt_paths options
--         msum ()
