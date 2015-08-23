module Main where

import           Language.Angler.Parser.Lexer (runLexer)

import           System.Environment           (getArgs)

import           Language.Angler.SrcLoc (unlocate)
import           Data.List              (intercalate)

main :: IO ()
main = do
        args <- getArgs
        input <- case args of
                f : _ -> readFile f
                _     -> getContents
        case runLexer input of
                Right ltks -> putStrLn . intercalate " " $ map (show . unlocate) ltks
                Left  err  -> print err
