module Main where

import           Language.Angler.Parser.Lexer  (evalLP)
import           Language.Angler.Parser.Parser (parseModule)
import           Language.Angler.SrcLoc        (SrcLoc(..))

import           System.Environment            (getArgs)
-- import           System.IO                     (openFile, stdin, IOMode(ReadMode))

import           Language.Angler.Error
import           Language.Angler.SrcLoc        (Located, unlocate)
import           Language.Angler.Parser.Lexer  (lexer)
import           Language.Angler.Parser.Token

import           Data.List                     (intercalate)

main :: IO ()
main = do
        args <- getArgs
        (input, filepath) <- case args of
                f : _ -> readFile f  >>= \i -> return (i, f )
                _     -> getContents >>= \i -> return (i, "")

        putStrLn "lexer\n"
        case runLexer input of
                Right ltks -> putStrLn . intercalate " " $ map (showNL . unlocate) ltks
                    where
                        showNL tk = (case tk of
                               TkSemicolon -> (++ "\n") . show
                               TkVLCurly   -> (++ "\n") . show
                               TkVRCurly   -> ("\n" ++) . show
                               _           -> show) tk

                Left  err  -> print err

        putStrLn "\nparser\n"
        print (evalLP input (SrcLoc filepath 1 1) parseModule)

runLexer :: String -> Either (Located Error) [Located Token]
runLexer input = evalLP input (SrcLoc "" 1 1) getTokens
    where
        getTokens = lexer return >>= cont
            where
                cont tk = case unlocate tk of
                        TkEOF -> return []
                        _     -> lexer cont >>= return . ((:) tk)
