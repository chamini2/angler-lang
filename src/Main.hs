module Main where

import           Language.Angler.AST
import           Language.Angler.Error
import           Language.Angler.Parser.Parser (lexProgram, parseProgram)
import           Language.Angler.Parser.Token  (Token, prettyShowTokens)
import           Language.Angler.Monad
import           Language.Angler.MixfixParser  (parseMixfix)
import           Language.Angler.Options
import           Language.Angler.SrcLoc
import           Language.Angler.ScopedTable   hiding (empty)
import qualified Language.Angler.ScopedTable   as ST (empty)

import           PrettyShow

import           Control.Lens
import           Control.Monad                 (forM, unless, when)

import           Data.Default                  (Default(..))
import           Data.Foldable                 (toList)

import           System.Console.GetOpt         (ArgOrder(..), getOpt)
import           System.Directory              (doesFileExist)
import           System.Exit                   (exitWith, ExitCode(..))
import           System.Environment            (getArgs)
import           System.FilePath               ((</>), addExtension, pathSeparator)
import           System.IO                     (Handle, IOMode(..), hGetContents,
                                                openFile, stdin)

import           Prelude                       hiding (IOError, elem)
import qualified Prelude                       as P (elem)

main :: IO ()
main = do
        args <- getArgs

        -- parsing options, getting a list of option actions
        let (optActions, nonOptions, optErrors) = getOpt Permute optionDescrs args
        strErrorsUnlessNull optErrors

        options <- foldActions optActions def
        print options

        (filepath, handle) <- case (nonOptions, view opt_stdin options) of
                ([ f ], False) -> do
                        h <- openModule f ((pshowError . IOError . OpenModule) f)
                        return (f, h)
                ([ ]  , True ) -> return ("<stdin>", stdin)
                ([ ]  , False) -> pshowError (IOError NoModules)
                (_ : _, _    ) -> pshowError (IOError TooManyModules)

        (table, ast) <- readModule options filepath handle
        return ()

readModule :: Options -> FilePath -> Handle -> IO (ScopedTable (), ModuleSpan)
readModule options filepath handle = do
        -- setting the flags for the import files
        let opts = options & (opt_stdin  .~ False)
                           . (opt_tokens .~ False)
                           . (opt_ast    .~ False)

        putStrLn ("Checking module (" ++ filepath ++ ")")

        input <- hGetContents handle
        let loc = startLoc filepath

        when (view opt_tokens options || view opt_verbose options) $ do
                putStr "\n\n***** lexer\n\n"
                case lexProgram input loc of
                        Right (ts,_) -> putStrLn (showTokens ts)
                            where
                                showTokens :: [Located Token] -> String
                                showTokens = prettyShowTokens . fmap (view loc_insd)
                        Left  _err   -> return ()


        ast <- case parseProgram input loc of
                Right (ast,ws) -> mapM_ print ws >> return ast
                Left  err      -> pshowError err

        -- imprtsTables <- readImports opts ast

        when (view opt_ast options || view opt_verbose options) $ do
                putStr "\n\n***** parser\n\n"
                putStrLn (prettyShow ast)

        ast' <- case parseMixfix ast of
                Right ast' -> return ast'
                Left  err  -> pshowErrors err >> return ast

        when (view opt_mixfix options || view opt_verbose options) $ do
                putStr "\n\n***** after mixfix parser\n\n"
                putStrLn (prettyShow ast')

        return (ST.empty, ast)
    {-where
        readImports :: Options -> ModuleSpan -> IO [(SymbolTableSpan, ModuleSpan)]
        readImports opts ast = forM (imports ast) $ \(Import path as mfs _) -> do
                handle' <- tryReadFile (qualifiedToPath path) (view opt_path opts)
                (table, ast') <- readModule opts path handle'

                let mfids = over (_Just.traverse) (view idn_str) mfs
                -- let mfids = traverseOf _Just (view idn_str) mfs
                let tableIm = filterByKey (checkIm mfids) table

                let exportsErrors = filter (not . flip elem tableIm) (maybe [] toList mfids)
                putStr "importing not exported functions: " >> print exportsErrors
                -- pshowErrorsUnlessNull (fmap (IOError . ImportingNoExport) exportsErrors)

                let tableAs = mapKeys (getAs as++) tableIm

                return (tableAs, ast)
            where
                imports :: ModuleSpan -> [ImportSpan]
                imports = toListOf (mod_imports.traverse)

                checkIm :: Foldable f => Maybe (f String) -> String -> Bool
                checkIm mfs k = maybe True (k `P.elem`) mfs

                getAs :: Maybe IdentifierSpan -> String
                getAs = maybe "" (view (idn_str.to (++".")))

                tryReadFile :: Foldable f => FilePath -> f FilePath -> IO Handle
                tryReadFile path = foldr go ((pshowError . IOError . OpenModule) path)
                    where
                        go :: FilePath -> IO Handle -> IO Handle
                        go dir = openModule (dir </> path)

                qualifiedToPath :: String -> FilePath
                qualifiedToPath = flip addExtension "ang" . foldr go ""
                    where
                        go :: Char -> FilePath -> FilePath
                        go c path = case c of
                                '.' -> pathSeparator : path
                                _   -> c             : path-}

-- maybe receive the ExitCode number instead of just plugging a one
strError :: String -> IO a
strError str = putStrLn str >> exitWith (ExitFailure 1)

strErrors :: Foldable f => f String -> IO a
strErrors = strError . concat

strErrorsUnlessNull :: Foldable f => f String -> IO ()
strErrorsUnlessNull es = unless (null es) (strErrors es)

pshowError :: PrettyShow s => s -> IO a
pshowError = strError . prettyShow

pshowErrors :: (Functor f, Foldable f, PrettyShow s) => f s -> IO a
pshowErrors = strErrors . fmap ((++"\n") . prettyShow)

pshowErrorsUnlessNull :: (Functor f, Foldable f, PrettyShow s) => f s -> IO ()
pshowErrorsUnlessNull = strErrorsUnlessNull . fmap ((++"\n") . prettyShow)

openModule :: FilePath -> IO Handle -> IO Handle
openModule path act = print path >> doesFileExist path >>= \ans ->
        if ans then openFile path ReadMode else act
