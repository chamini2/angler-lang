module Main where

import           Language.Angler.Parser.Lexer (runLexer)

main :: IO ()
main = do
        input <- getContents
        either print (mapM_ print) (runLexer input)
