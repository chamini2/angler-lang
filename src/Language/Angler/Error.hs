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
  deriving Show

data ParseError
  = PErr                String
  | PErrEmptyLayout
  deriving Show
