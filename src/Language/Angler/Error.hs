{-# LANGUAGE FlexibleInstances #-}

module Language.Angler.Error
        ( Error(..)
        , LexError(..)
        , ParseError(..)
        ) where

import Language.Angler.SrcLoc              (Located(Loc), SrcSpan(SrcSpanNoInfo))

import qualified Control.Monad.Error.Class as ErrorC

data Error
  = LexError    LexError
  | ParseError  ParseError
  | Err         String
  deriving Show

instance ErrorC.Error (Located Error) where
        noMsg  = Loc SrcSpanNoInfo (Err [])
        strMsg = Loc SrcSpanNoInfo . Err

data LexError
  = LErr                        String
  | LErrUnexpectedCharacter     Char
  deriving Show

data ParseError
  = PErr                String
  | PErrEmptyLayout
  deriving Show
