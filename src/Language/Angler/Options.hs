{-# LANGUAGE TemplateHaskell #-}
module Language.Angler.Options
        ( Options(..), def
        , optionDescrs
        -- Lenses
        , opt_warnings , opt_tokens , opt_ast , opt_stdlib, opt_paths
        ) where

import           Control.Lens

import           Data.Default                  (Default(..))

import           System.Console.GetOpt         (ArgDescr(..), OptDescr(..),
                                                usageInfo)
import           System.Exit                   (exitWith, ExitCode(..))
import           System.Directory              (doesDirectoryExist)

data Options
  = Options
        { _opt_warnings  :: Bool
        , _opt_tokens    :: Bool                 -- maybe change to _opt_verbose
        , _opt_ast       :: Bool
        -- , _opt_symbols   :: Bool
        -- , _opt_execute   :: Bool

        -- path managing for module searching
        , _opt_stdlib   :: Bool
        , _opt_paths     :: [FilePath]
        }
  deriving (Eq, Show)

makeLenses ''Options

instance Default Options where
    def = Options
        { _opt_warnings = True
        , _opt_tokens   = False
        , _opt_ast      = False
        -- , _opt_symbols  = False
        -- , _opt_execute  = True
        , _opt_stdlib  = True
        , _opt_paths    = ["./"]
        }

optionDescrs :: [OptDescr (Options -> IO Options)]
optionDescrs =
        [ Option ['h'] ["help"]        (NoArg (const (putStrLnSuccess help)))
                "shows this help message"

        , Option ['v'] ["version"]     (NoArg (const (putStrLnSuccess version)))
                "shows version number"

        , Option ['w'] ["warnings"]    (NoArg (optBool opt_warnings True))
                "shows all warnings (default)"

        , Option ['W'] ["no-warnings"] (NoArg (optBool opt_warnings False))
                "suppress all warnings"

        , Option []    ["tokens"]      (NoArg (optBool opt_tokens True))
                "shows the tokens recognized by the lexer"

        , Option []    ["no-tokens"]   (NoArg (optBool opt_tokens False))
                "avoids showing the tokens recognized by the lexer (default)"

        , Option []    ["ast"]         (NoArg (optBool opt_ast True))
                "shows the AST parsed"

        , Option []    ["no-ast"]      (NoArg (optBool opt_ast False))
                "avoids showing the AST parsed (default)"

        , Option []    ["stdlib"]      (NoArg (optBool opt_stdlib True))
                "adds the standard library to the path (default)"

        , Option []    ["no-stdlib"]   (NoArg (optBool opt_stdlib False))
                "removes the standard library from the path"

        , Option ['p'] ["path"]        (ReqArg optPaths "PATH")
                "adds a directory to the path"
        ]
    where
        -- optBool :: Lens' Options Bool -> Bool -> Options -> IO Options
        optBool lns b = return . set lns b

        optPaths :: String -> Options -> IO Options
        optPaths dir opt = do
                dirExists <- doesDirectoryExist dir
                if dirExists
                        then return (over opt_paths (dir :) opt)
                        else ioError (userError (dir ++ " is not a valid directory"))

        putStrLnSuccess :: String -> IO Options
        putStrLnSuccess str = putStrLn str >> exitWith ExitSuccess

        help :: String
        help = flip usageInfo optionDescrs "usage: angler [OPTIONS...] [FILE]\n" ++
                "\twhen running angler without arguments, the interpreter " ++
                "consumes the standard input until it receives an EOF ('^D') character."

        version :: String
        version = "Angler version 0.1.0.0"
