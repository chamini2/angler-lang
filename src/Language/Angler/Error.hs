module Language.Angler.Error
        ( Warning(..)
        , Error(..)
        , LexError(..)
        , ParseError(..)
        , CheckError(..)
        , IOError(..)
        ) where

import           PrettyShow

import           Prelude hiding (IOError)

ticks :: String -> String
ticks s = "`" ++ s ++ "`"

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
                LErrUnexpectedCharacter c -> "unexpected character " ++ ticks [c]
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
                PErrUnexpectedToken  tk -> "unexpected token " ++ ticks tk
                PErrEmptyLayoutAfter  w -> "empty layout after " ++ ticks w
                PErrExpectingIn     e c -> "expected " ++ ticks e ++ " in " ++ ticks c
                PErrExpectedAfter   e c -> "expected " ++ ticks e ++ " after " ++ ticks c
                PErrUnexpectedAfter u c -> "unexpected " ++ ticks u ++ " after " ++ ticks c
                PErrNoIn            e c -> "no " ++ ticks e ++ " in " ++ ticks c

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

  -- compact
  | CErrExpectingInsteadOf      String String

instance Show CheckError where
        show ce = case ce of
                CErr                     str -> str
                CErrAlreadyInSymbolTable idn -> "identifier '" ++ idn ++ "' has already been declared at this scope"
                CErrNotInSymbolTable     idn -> "identifier '" ++ idn ++ "' is not in the symbol table"
                CErrExpected              tk -> "in mixfix parser, expected " ++ tk
                CErrUnexpected            tk -> "in mixfix parser, unexpected " ++ tk
                CErrExpectingInsteadOf   e f -> "expecting " ++ e ++ " instead of " ++ f


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
