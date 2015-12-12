{-# LANGUAGE RankNTypes #-}

module Language.Angler.Options where

import           Language.Angler.Monad         (foldActions)

import           Control.Lens

import           Data.Default                  (Default(..))

import           System.Console.GetOpt         (ArgDescr(..), OptDescr(..), usageInfo)
import           System.Exit                   (exitWith, ExitCode(..))
import           System.Directory              (doesDirectoryExist)

data Options
  = Options
        { _opt_warnings :: Bool

        -- stages printing
        , _opt_options  :: Bool
        , _opt_tokens   :: Bool
        , _opt_ast      :: Bool
        , _opt_mixfix   :: Bool
        , _opt_compact  :: Bool

        -- behaviour
        , _opt_stdin    :: Bool
        -- , _opt_symbols  :: Bool
        -- , _opt_execute  :: Bool

        -- path managing for module searching
        , _opt_stdlib   :: Bool
        , _opt_path     :: [FilePath]
        }

makeLenses ''Options

instance Show Options where
        show opt = showBoolOpts [ ("warnings", opt_warnings)
                                , ("stdin", opt_stdin)
                                , ("stdlib", opt_stdlib) ] ++
                   "path: " ++ showPaths (view opt_path opt) ++ "\n" ++
                   showBoolOpts [ ("options", opt_options)
                                , ("tokens", opt_tokens)
                                , ("ast", opt_ast)
                                , ("mixfix", opt_mixfix)
                                , ("compact", opt_compact) ]
            where
                showBoolOpts :: [(String, Getting Bool Options Bool)] -> String
                showBoolOpts = concatMap (uncurry showBoolOpt)
                showBoolOpt :: String -> Getting Bool Options Bool -> String
                showBoolOpt desc l = desc ++ ": " ++ (if view l opt then "ON" else "OFF") ++ "\n"
                showPaths :: [FilePath] -> String
                showPaths = concat . fmap quoteFile
                    where
                        quoteFile :: FilePath -> FilePath
                        quoteFile f = " " ++ if elem ' ' f then "\"" ++ f ++ "\"" else f

instance Default Options where
    def = Options
        { _opt_warnings = True
        , _opt_options  = False
        , _opt_tokens   = False
        , _opt_ast      = False
        , _opt_mixfix   = False
        , _opt_compact  = False
        , _opt_stdin    = False
        -- , _opt_symbols  = False
        -- , _opt_execute  = True
        , _opt_stdlib   = True
        , _opt_path     = ["."]                 -- search path
        }

optionDescrs :: [OptDescr (Options -> IO Options)]
optionDescrs =
        [ Option ['h'] ["help"]          ((NoArg . const . putLnSuccess) help)
                "shows this help message"

        , Option ['v'] ["version"]       ((NoArg . const . putLnSuccess) version)
                "shows version number"

        , Option ['w'] ["warnings"]      (NoArg (optBool True  opt_warnings))
                "shows all warnings (default)"
        , Option ['W'] ["no-warnings"]   (NoArg (optBool False opt_warnings))
                "suppress all warnings"

        , Option ['i'] ["stdin"]         (NoArg (optBool True  opt_stdin))
                "reads the program to interpret from standard input until EOF ('^D')"
        , Option []    ["no-stdin"]      (NoArg (optBool False opt_stdin))
                "reads the program from a file passed as argument (default)"

        , Option []    ["stdlib"]        (NoArg (optBool True  opt_stdlib))
                "adds the standard library to the path (default)"
        , Option []    ["no-stdlib"]     (NoArg (optBool False opt_stdlib))
                "removes the standard library from the path"

        , Option ['p'] ["path"]          (ReqArg optPath "PATH")
                "adds a directory to the path"

        , Option ['V'] ["verbose"]       (NoArg (optVerbose True))
                ("to show output for the several stages of interpreting, " ++
                "this turns on the following flags: options, tokens, ast, mixfix, compact")
        , Option []    ["no-verbose"]    (NoArg (optVerbose False))
                "avoids showing output for the several stages of interpreting (default)"

        , Option []    ["options"]       (NoArg (optBool True  opt_options))
                "shows the options passed to the program"
        , Option []    ["no-options"]    (NoArg (optBool False opt_options))
                "avoids showing the options passed to the program (default)"

        , Option []    ["tokens"]        (NoArg (optBool True  opt_tokens))
                "shows the tokens recognized by the lexer"
        , Option []    ["no-tokens"]     (NoArg (optBool False opt_tokens))
                "avoids showing the tokens recognized by the lexer (default)"

        , Option []    ["ast"]           (NoArg (optBool True  opt_ast))
                "shows the parsed AST"
        , Option []    ["no-ast"]        (NoArg (optBool False opt_ast))
                "avoids showing the parsed AST (default)"

        , Option []    ["mixfix"]        (NoArg (optBool True  opt_mixfix))
                "shows the parsed AST after passing the mixfix parser"
        , Option []    ["no-mixfix"]     (NoArg (optBool False opt_mixfix))
                "avoids showing the parsed AST after passing the mixfix parser (default)"

        , Option []    ["compact-ast"]    (NoArg (optBool True  opt_compact))
                "shows the AST after compacting it"
        , Option []    ["no-compact-ast"] (NoArg (optBool False opt_compact))
                "avoids showing AST after compacting it (default)"
        ]
    where
        optVerbose :: Bool -> Options -> IO Options
        optVerbose b = foldActions $ fmap (optBool b) [opt_options, opt_tokens, opt_ast, opt_mixfix, opt_compact]

        optBool :: Bool -> ASetter' Options Bool -> Options -> IO Options
        optBool b lns = return . set lns b

        optPath :: String -> Options -> IO Options
        optPath dir opt = doesDirectoryExist dir >>= \ans ->
                if ans then return (over opt_path (|> dir) opt)
                       else ioError (userError ("directory '" ++ dir ++ "' does not exist"))

        putLnSuccess :: String -> IO Options
        putLnSuccess str = putStrLn str >> exitWith ExitSuccess

        help :: String
        help = flip usageInfo optionDescrs "usage: angler [OPTIONS...] [FILE]\n"

        version :: String
        version = "Angler version 0.1.0.0"
