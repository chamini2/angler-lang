module Language.Angler.Parser.Token where

data Token
  -- identifiers
  -- = TkTypeId            String
  -- | TkFunctionId        String
  = TkIdentifier        { tkId :: String }
  -- | TkImportPath        FilePath
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
  | TkWhere
  | TkForall
  | TkExists
  | TkSelect
  -- | TkBehaviour
  -- | TkOn
  -- | TkIs

  -- reserved symbols
  | TkColon
  | TkSemicolon
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
        -- TkTypeId     str   -> showId str
        -- TkFunctionId str   -> showId str
        TkIdentifier str   -> showId str
        -- TkImportPath str   -> showId str
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
        TkWhere            -> "where"
        TkForall           -> "forall"
        TkExists           -> "exists"
        TkSelect           -> "select"
        -- TkOn               -> "on"
        -- TkIs               -> "is"

        TkColon            -> ":"
        TkSemicolon        -> ";"
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

