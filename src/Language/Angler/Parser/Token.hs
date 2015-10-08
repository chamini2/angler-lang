module Language.Angler.Parser.Token where

import           PrettyShow

data Token
  -- identifiers
  = TkIdentifier        { tkId :: String }
  | TkQualified         { tkId :: String }

  -- comments
  | TkLineComment       String
  | TkBlockComment      String

  -- literals
  | TkInteger           { tkInt    :: Int    }
  | TkChar              { tkChar   :: Char   }
  | TkString            { tkString :: String }

  -- layouts
  | TkVLCurly
  | TkVRCurly
  | TkVSemicolon

  -- reserved words
  | TkExport
  | TkImport
  | TkAs
  | TkOpen
  | TkReopen
  | TkClosed
  | TkWith
  | TkLet
  | TkIn
  | TkWhere
  | TkForall
  | TkExists
  | TkSelect
  -- | TkBehaviour
  -- | TkOn
  -- | TkIs
  | TkOperator
  | TkPrefix
  | TkPostfix
  | TkInfixL
  | TkInfixR
  | TkInfixN

  -- reserved symbols
  | TkColon
  | TkDot
  | TkArrow
  | TkBackslash
  | TkEquals
  | TkComma
  -- | TkAt
  | TkLParen
  | TkRParen
  | TkLCurly
  | TkRCurly
  | TkUnderscore

  -- interpreter stuff
  | TkEOF

instance Show Token where
        show tk = case tk of
                TkIdentifier str   -> showId str
                TkQualified  str   -> showId str

                TkLineComment str  -> "--" ++ str
                TkBlockComment str -> "{-" ++ str ++ "-}"

                TkInteger int      -> show int
                TkChar    chr      -> show chr
                TkString  str      -> show str

                TkVLCurly          -> "{^"
                TkVRCurly          -> "^}"
                TkVSemicolon       -> "^;"

                TkExport           -> "export"
                TkImport           -> "import"
                TkAs               -> "as"
                TkOpen             -> "open"
                TkReopen           -> "reopen"
                TkClosed           -> "closed"
                TkWith             -> "with"
                TkLet              -> "let"
                TkIn               -> "in"
                TkWhere            -> "where"
                TkForall           -> "forall"
                TkExists           -> "exists"
                TkSelect           -> "select"
                -- TkBehaviour        -> "behaviour"
                -- TkOn               -> "on"
                -- TkIs               -> "is"
                TkOperator         -> "operator"
                TkPrefix           -> "prefix"
                TkPostfix          -> "postfix"
                TkInfixL           -> "infixL"
                TkInfixR           -> "infixR"
                TkInfixN           -> "infixN"

                TkColon            -> ":"
                TkDot              -> "."
                TkArrow            -> "->"
                TkBackslash        -> "\\"
                TkEquals           -> "="
                TkComma            -> ","
                -- TkAt               -> "@"
                TkLParen           -> "("
                TkRParen           -> ")"
                TkLCurly           -> "{"
                TkRCurly           -> "}"
                TkUnderscore       -> "_"

                TkEOF                -> "<eof>"
            where
                showId str = "«" ++ str ++ "»"

instance PrettyShow Token where
        pshow tk = case tk of
                TkVLCurly          -> string "{^" >> line
                TkVRCurly          -> line >> string "^}"
                TkVSemicolon       -> string "^;" >> line
                _                  -> string (show tk)
