module Language.Angler.Parser.Token where

data Token
  = TkIdentifier String
  -- | TkNewLine

  -- layouts
  | TkVLCurly
  | TkVRCurly
  | TkSemicolon

  -- reserved words
  | TkWhere
  | TkForall
  | TkExists
  | TkWith
  | TkOn
  | TkIs

  -- reserved symbols
  | TkArrow
  | TkColon
  | TkEquals

  -- interpreter stuff
  | TkEOF

instance Show Token where
  show tk = case tk of
         TkIdentifier str -> "«" ++ str ++ "»"
         TkVLCurly        -> "{"
         TkVRCurly        -> "}"
         TkSemicolon      -> ";"
         TkWhere          -> "where"
         TkForall         -> "forall"
         TkExists         -> "exists"
         TkWith           -> "with"
         TkOn             -> "on"
         TkIs             -> "is"
         TkArrow          -> "->"
         TkColon          -> ":"
         TkEquals         -> "="
         TkEOF            -> "<eof>"

