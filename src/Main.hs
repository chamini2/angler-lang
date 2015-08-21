module Main where

import           Language.Angler.Parser.Lexer (runLexer)
import           Language.Angler.Parser.Token

--import           Data.Either (either)

main :: IO ()
main = do
        input <- getContents
        either print (mapM_ print) (runLexer input)
