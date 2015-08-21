module Language.Angler.Parser.Token where

data Token
  = TkIdentifier String
  | TkNewLine

  -- Layout / Context
  | TkVLCurly
  | TkVRCurly
  | TkSemicolon

  -- Reserved words
  | TkWhere
  | TkForall
  | TkExists
  | TkWith
  | TkOn
  | TkIs

  -- Interpreter
  | TkEOF
  deriving Show
