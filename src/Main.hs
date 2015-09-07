module Main where

import           Language.Angler.AST           (prettyShow)
import           Language.Angler.Parser.Lexer  (evalLP)
import           Language.Angler.Parser.Parser (parseModule)
import           Language.Angler.SrcLoc        (SrcLoc(..))

import           Control.Monad                 (unless, when)
import           System.Console.GetOpt         (ArgDescr (..), ArgOrder (..),
                                                OptDescr (..), getOpt, usageInfo)
import           System.Environment            (getArgs)
import           System.Exit                   (exitWith, ExitCode(..))
import           System.Directory              (doesDirectoryExist)
-- import           System.IO                     (Handle, IOMode(..), openFile, stdin, stdout, stderr)
import           System.IO


-- import           Language.Angler.Error
import           Language.Angler.SrcLoc        (unlocate)
import           Language.Angler.Parser.Lexer  (lexTokens)
import           Language.Angler.Parser.Token

import           Data.List                     (intercalate)

main :: IO ()
main = do
        args <- getArgs

        -- Parse options, getting a list of option actions
        let (optActions, nonOptions, optErrors) = getOpt Permute optionDescrs args

        unless (null optErrors) $ ioError (userError (concat optErrors))

        options <- foldl (>>=) (return initialOptions) optActions

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

        when (opt_tokens options) $ do
                putStrLn "\n\n***** lexer\n"
                case evalLP' lexTokens of
                        Right ltks -> putStrLn . intercalate " " $ map (showNL . unlocate) ltks
                            where
                                showNL tk = (case tk of
                                       TkVSemicolon -> (++ "\n") . show
                                       TkVLCurly    -> (++ "\n") . show
                                       TkVRCurly    -> ("\n" ++) . show
                                       _            -> show) tk

                        _ -> return ()

        when (opt_ast options) $ do
                putStrLn "\n\n***** parser\n"
                case evalLP' parseModule of
                        Right lmod -> putStrLn (prettyShow lmod)
                        Left  err  -> print err

        -- symbols <- readModule options modrelpath
    where


-- readModule :: Options -> FilePath -> IO a
-- readModule options modrelpath = do
--         let paths = opt_paths options
--         msum ()


--------------------------------------------------------------------------------
-- Options handling

data Options
  = Options
        { opt_warnings  :: Bool
        , opt_tokens    :: Bool                 -- maybe change to verbose
        , opt_ast       :: Bool
        -- , opt_symbols   :: Bool
        -- , opt_execute   :: Bool

        -- path managing for module searching
        , opt_stdpath   :: Bool
        , opt_paths     :: [FilePath]
        }
  deriving (Eq, Show)

initialOptions :: Options
initialOptions = Options
        { opt_warnings = True
        , opt_tokens   = False
        , opt_ast      = False
        -- , opt_symbols  = False
        -- , opt_execute  = True
        , opt_stdpath  = True
        , opt_paths    = []
        }

optionDescrs :: [OptDescr (Options -> IO Options)]
optionDescrs =
        [ Option ['h'] ["help"]        (NoArg (const (printSuccess help)))
                "shows this help message"

        , Option ['v'] ["version"]     (NoArg (const (printSuccess version)))
                "shows version number"

        , Option ['w'] ["warnings"]    (NoArg (optWarnings True))
                "shows all warnings (default)"

        , Option ['W'] ["no-warnings"] (NoArg (optWarnings False))
                "suppress all warnings"

        , Option []    ["tokens"]      (NoArg (optTokens True))
                "shows the tokens recognized by the lexer"

        , Option []    ["no-tokens"]   (NoArg (optTokens False))
                "avoids showing the tokens recognized by the lexer (default)"

        , Option []    ["ast"]         (NoArg (optAst True))
                "shows the AST parsed"

        , Option []    ["no-ast"]      (NoArg (optAst False))
                "avoids showing the AST parsed (default)"

        , Option []    ["stdlib"]      (NoArg (optStdpath True))
                "adds the standard library to the path (default)"

        , Option []    ["no-stdlib"]   (NoArg (optStdpath False))
                "removes the standard library from the path"

        , Option ['p'] ["path"]        (ReqArg optPaths "PATH")
                "adds a directory to the path"
        ]
    where
        optWarnings :: Bool -> Options -> IO Options
        optWarnings b opt = return opt { opt_warnings = b }

        optTokens :: Bool -> Options -> IO Options
        optTokens b opt = return opt { opt_tokens = b }

        optAst :: Bool -> Options -> IO Options
        optAst b opt = return opt { opt_ast = b }

        optStdpath :: Bool -> Options -> IO Options
        optStdpath b opt = return opt { opt_stdpath = b }

        optPaths :: String -> Options -> IO Options
        optPaths dir opt = do
                dirExists <- doesDirectoryExist dir
                if dirExists
                        then return opt { opt_paths = opt_paths opt ++ [dir] }
                        else ioError (userError (dir ++ " is not a valid directory"))

        printSuccess :: String -> IO Options
        printSuccess str = putStrLn str >> exitWith ExitSuccess

        help :: String
        help = flip usageInfo optionDescrs "usage: angler [OPTIONS...] [FILE]\n" ++
                "\twhen running angler without arguments, the interpreter " ++
                "consumes the standard input until it receives an EOF ('^D') character."

        version :: String
        version = "Angler version 0.1.0.0"
