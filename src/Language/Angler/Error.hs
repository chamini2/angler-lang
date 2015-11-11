module Language.Angler.Error where

import           PrettyShow

import           Prelude hiding (IOError)

data Warning
  = Warn                        String
  | TabCharacter

instance Show Warning where
        show w = "WARNING: " ++ case w of
                Warn str     -> str
                TabCharacter -> "tab character, use spaces instead"

data Error
  = Err         String
  | LexError    LexError
  | ParseError  ParseError
  | CheckError  CheckError
  | IOError     IOError

instance Show Error where
        show e = case e of
                Err       str -> "ERROR: " ++ str
                LexError   le -> "LEXER ERROR: " ++ show le
                ParseError pe -> "PARSE ERROR: " ++ show pe
                CheckError ce -> "ERROR: " ++ show ce
                IOError   ioe -> "IO ERROR: " ++ show ioe

instance PrettyShow Error where
        pshow = string . show

data LexError
  = LErr                        String
  | LErrUnexpectedCharacter     Char
  | LErrUnterminatedComment

instance Show LexError where
        show le = case le of
                LErr                  str -> str
                LErrUnexpectedCharacter c -> "unexpected character " ++ [c]
                LErrUnterminatedComment   -> "unterminated comment"

instance PrettyShow LexError where
        pshow = string . show

data ParseError
  = PErr                        String
  | PErrUnexpectedToken         String
  | PErrEmptyLayoutAfter        String
  | PErrExpectingIn             String String
  | PErrExpectedAfter           String String
  | PErrUnexpectedAfter         String String
  | PErrNoIn                    String String

instance Show ParseError where
        show pe = case pe of
                PErr                str -> str
                PErrUnexpectedToken  tk -> "unexpected token " ++ tk
                PErrEmptyLayoutAfter  w -> "empty layout after " ++ w
                PErrExpectingIn     e c -> "expected " ++ e ++ " in " ++ c
                PErrExpectedAfter   e c -> "expected " ++ e ++ " after " ++ c
                PErrUnexpectedAfter u c -> "unexpected " ++ u ++ " after " ++ c
                PErrNoIn            e c -> "no " ++ e ++ " in " ++ c

instance PrettyShow ParseError where
        pshow = string . show

data CheckError
  = CErr                        String

  -- symbol table
  | CErrAlreadyInSymbolTable    String
  | CErrNotInSymbolTable        String

  -- mixfix parser
  | CErrExpected                String
  | CErrUnexpected              String

instance Show CheckError where
        show ce = case ce of
                CErr                     str -> str
                CErrAlreadyInSymbolTable idn -> "identifier '" ++ idn ++ "' has already been declared at this scope"
                CErrNotInSymbolTable     idn -> "Identifier '" ++ idn ++ "' is not in the symbol table"
                CErrExpected              tk -> "in mixfix parser, expected " ++ tk
                CErrUnexpected            tk -> "in mixfix parser, unexpected " ++ tk

instance PrettyShow CheckError where
        pshow = string . show

data IOError
  = IOErr                       String

  -- import
  | ImportingNoExport           String

  -- modules / files
  | OpenModule                  String
  | TooManyModules
  | NoModules
  deriving Show

instance PrettyShow IOError where
        pshow = string . show
