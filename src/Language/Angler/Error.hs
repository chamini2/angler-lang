module Language.Angler.Error
        ( Warning(..)
        , Error(..)
        , LexError(..)
        , ParseError(..)
        , CheckError(..)
        , IOError(..)
        ) where

import           PrettyShow

import           Data.List      (intercalate)

import           Prelude        hiding (IOError)

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
                LErrUnexpectedCharacter c -> "unexpected character " ++ ticks (caseShow c)
                LErrUnterminatedComment   -> "unterminated comment"
            where
                caseShow :: Char -> String
                caseShow chr = case chr of
                        '\a' -> "\\a"
                        '\b' -> "\\b"
                        '\f' -> "\\f"
                        '\n' -> "\\n"
                        '\r' -> "\\r"
                        '\t' -> "\\t"
                        '\v' -> "\\v"
                        _ -> [chr]

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
  | CErrMixfix                  [String] [String] [String]

  -- compact
  | CErrExpectingInsteadOf      String String

  -- typecheck
  | CErrTooManyArguments        String
  | CErrCouldNotInferType
  | CErrCannotApply             String String
  | CErrTypeError               String String

instance Show CheckError where
        show ce = case ce of
                CErr                     str -> str
                -- symbol table
                CErrAlreadyInSymbolTable idn -> "identifier " ++ ticks idn ++ " has already been declared at this scope"
                CErrNotInSymbolTable     idn -> "identifier " ++ ticks idn ++ " is not in the symbol table"
                -- mixfix parser
                CErrMixfix          es us ms -> "in mixfix parser,\n\t" ++
                                                caseMsg "unexpected " us ++
                                                caseMsg "expected " es ++
                                                concatMap (++"\n") ms
                    where
                        caseMsg :: String -> [String] -> String
                        caseMsg msg strs = case strs of
                                [] -> ""
                                _  -> msg ++ intercalate " or " strs ++ "\n\t"
                -- compact
                CErrExpectingInsteadOf   e f -> "expecting " ++ ticks e ++ " instead of " ++ ticks f
                -- typecheck
                CErrTooManyArguments       f -> "too many arguments for function " ++ ticks f
                CErrCouldNotInferType        -> "could not infert type for expression"
                CErrCannotApply          e f -> "cannot apply " ++ ticks e ++ " to " ++ ticks f
                CErrTypeError            t u -> ticks t ++ " does not unify with " ++ ticks u

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
