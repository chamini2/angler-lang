module Language.Angler.Parser.Token where

data Token
  = TkIdentifier String
  | TkQualified  String
  -- | TkNewLine

  -- layouts
  | TkVLCurly
  | TkVRCurly
  | TkSemicolon

  -- reserved words
  | TkExport
  | TkImport
  | TkAs
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
  | TkComma
  | TkLParen
  | TkRParen
  | TkLCurly
  | TkRCurly
  | TkUnderscore

  -- interpreter stuff
  | TkEOF

instance Show Token where
  show tk = case tk of
        TkIdentifier str -> "«" ++ str ++ "»"
        TkQualified  str -> "«" ++ str ++ "»"
        -- TkNewLine        -> "<nl>"
        TkVLCurly        -> "{^"
        TkVRCurly        -> "^}"
        TkSemicolon      -> ";"
        TkExport         -> "export"
        TkImport         -> "import"
        TkAs             -> "as"
        TkWhere          -> "where"
        TkForall         -> "forall"
        TkExists         -> "exists"
        TkWith           -> "with"
        TkOn             -> "on"
        TkIs             -> "is"
        TkArrow          -> "->"
        TkColon          -> ":"
        TkEquals         -> "="
        TkComma          -> ","
        TkLParen         -> "("
        TkRParen         -> ")"
        TkLCurly         -> "{"
        TkRCurly         -> "}"
        TkUnderscore     -> "_"
        TkEOF            -> "<eof>"

