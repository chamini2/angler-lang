module Language.Angler.Error where

import           Prelude hiding (IOError)

data Warning
  = TabCharacter
  | Warn                        String
  deriving Show

data Error
  = LexError    LexError
  | ParseError  ParseError
  | CheckError  CheckError
  | IOError     IOError
  | Err         String
  deriving Show

data LexError
  = LErr                        String
  | LErrUnexpectedCharacter     Char
  | LErrUnterminatedComment
  deriving Show

data ParseError
  = PErr                        String
  | PErrEmptyLayoutAfter        String
  | PErrExpectingIn             String String

  -- expressions
  | PErrNoArgumentsIn           String
  | PErrNoExpressionIn          String
  | PErrNoVariablesIn           String
  | PErrNoVariableIn            String
  | PErrNoBindIn                String
  deriving Show

data CheckError
  = CErr                        String
  | AlreadyInSymbolTable        String
  deriving Show

data IOError
  = IOErr                       String

  -- import
  | ImportingNoExport           String

  -- modules / files
  | OpenModule                  String
  | TooManyModules
  | NoModules
  deriving Show
