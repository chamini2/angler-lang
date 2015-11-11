module Main where

import           Language.Angler.AST
import           Language.Angler.Error
import           Language.Angler.Parser.Parser (lexProgram, parseProgram)
import           Language.Angler.Parser.Token  (Token, prettyShowTokens)
import           Language.Angler.Monad
import           Language.Angler.MixfixParser  (parseMixfix)
import           Language.Angler.Compact       (compactAST)
import           Language.Angler.Options
import           Language.Angler.SrcLoc
import           Language.Angler.ScopedTable   hiding (empty)

import           PrettyShow

import           Control.Lens

import           Control.Monad                 (forM, unless, when)

import           Control.Exception             (evaluate)

import           Data.Default                  (Default(..))
import           Data.Foldable                 (toList)

import           System.Console.GetOpt         (ArgOrder(..), getOpt)
import           System.Clock                  (Clock(..), diffTimeSpec, getTime, timeSpecAsNanoSecs)
import           System.Directory              (doesFileExist)
import           System.Exit                   (exitWith, ExitCode(..))
import           System.Environment            (getArgs)
import           System.FilePath               ((</>), addExtension, pathSeparator)
import           System.IO                     (Handle, IOMode(..), hGetContents, openFile, stdin)

import           Prelude                       hiding (IOError, elem)
import qualified Prelude                       as P (elem)

main :: IO ()
main = do
        args <- getArgs

        -- parsing options, getting a list of option actions
        let (optActions, nonOptions, optErrors) = getOpt Permute optionDescrs args
        putErrorsUnlessNull optErrors

        options <- foldActions optActions def
        print options

        (filepath, handle) <- case (nonOptions, view opt_stdin options) of
                ([f], False) -> do
                        h <- (openModule f . pshowError . IOError . OpenModule) f
                        return (f, h)
                ([] , True ) -> return ("<stdin>", stdin)
                ([] , False) -> pshowError (IOError NoModules)
                (_  , _    ) -> pshowError (IOError TooManyModules)

        _ <- readModule options filepath handle
        return ()

readModule :: Options -> FilePath -> Handle -> IO (ScopedTable (), ModuleSpan)
readModule options filepath handle = do
        -- setting the flags for the import files
        let imprtsOpts = options & (opt_stdin  .~ False)
                                 . (opt_tokens .~ False)
                                 . (opt_ast    .~ False)
                                 . (opt_mixfix .~ False)

        putStrLn ("Checking module (" ++ filepath ++ ")")

        input <- hGetContents handle
        let loc = startLoc filepath

        when (view opt_tokens options || view opt_verbose options) $ do
                printStage "lexer" Nothing
                (_,lexSecs) <- stopwatch $ case lexProgram input loc of
                        Right (ts,_) -> evaluate ts >>= putStrLn . showTokens
                            where
                                showTokens :: [Located Token] -> String
                                showTokens = prettyShowTokens . fmap (view loc_insd)
                        Left  _err   -> return ()
                printStage "lexer" (Just lexSecs)

        (parseAST,parseSecs) <- stopwatch $ case parseProgram input loc of
                Right (ast,ws) -> mapM_ print ws >> evaluate ast
                Left  err      -> pshowError err
        when (view opt_ast options || view opt_verbose options) $ do
                printStage "parser" Nothing
                putStrLn (prettyShow parseAST)
                printStage "parser" (Just parseSecs)

        -- imprts <- readImports imprtsOpts ast

        (mixfixAST,mixfixSecs) <- stopwatch $ case parseMixfix parseAST of
                Right ast  -> evaluate ast
                Left  errs -> pshowErrors errs
        when (view opt_mixfix options || view opt_verbose options) $ do
                printStage "mixfix parser" Nothing
                putStrLn (prettyShow mixfixAST)
                printStage "mixfix parser" (Just mixfixSecs)

        -- (compactedAST,compactedSecs) <- stopwatch $ evaluate (compactAST mixfixAST)
        -- when (view opt_compact options || view opt_verbose options) $ do
        --         printStage "compact" Nothing
        --         putStrLn (prettyShow compactedAST)
        --         printStage "compact" (Just compactedSecs)

        return (undefined, undefined)
    where
        printStage :: String -> Maybe Double -> IO ()
        printStage stage msecs = do
                let secs = case msecs of
                        Just s  -> " (" ++ show s ++ " s)"
                        Nothing -> ""
                putStrLn ("\n********** " ++ stage ++ secs ++ "\n")

        stopwatch :: IO a -> IO (a, Double)
        stopwatch act = do
                start <- getTime ProcessCPUTime
                res   <- act
                end   <- getTime ProcessCPUTime
                let nanoSecs = timeSpecAsNanoSecs (diffTimeSpec start end)
                    secs     = fromInteger nanoSecs / 10 ^ 9
                return (res, secs)
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
                getAs = maybe "" (views idn_str (++"."))

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

-- maybe receive the ExitCode number instead of just hard-coding one in
putError :: String -> IO a
putError str = putStrLn str >> exitWith (ExitFailure 1)

putErrors :: Foldable f => f String -> IO a
putErrors = putError . concat

putErrorsUnlessNull :: Foldable f => f String -> IO ()
putErrorsUnlessNull es = unless (null es) (putErrors es)

pshowError :: PrettyShow s => s -> IO a
pshowError = putError . prettyShow

pshowErrors :: (Functor f, Foldable f, PrettyShow s) => f s -> IO a
pshowErrors = putErrors . fmap ((++"\n") . prettyShow)

pshowErrorsUnlessNull :: (Functor f, Foldable f, PrettyShow s) => f s -> IO ()
pshowErrorsUnlessNull = putErrorsUnlessNull . fmap ((++"\n") . prettyShow)

openModule :: FilePath -> IO Handle -> IO Handle
openModule path act = print path >> doesFileExist path >>= \ans ->
        if ans then openFile path ReadMode else act
