{-# LANGUAGE FlexibleInstances #-}

module Language.Angler.Error
        ( Error(..)
        , LexError(..)
        , ParseError(..)
        ) where

data Error
  = LexError    LexError
  | ParseError  ParseError
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
