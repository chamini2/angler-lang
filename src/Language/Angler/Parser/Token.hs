module Language.Angler.Parser.Token where

import           PrettyShow

data Fixity
  = Prefix
  | Postfix
  | Infix
  | Closedfix
  | Nofix
  deriving Eq

instance Show Fixity where
        show fix = case fix of
                Infix     -> "infix"
                Prefix    -> "prefix"
                Postfix   -> "postfix"
                Closedfix -> "closed"
                Nofix     -> "no-fix"

data Token
  -- identifiers
  = TkIdentifier        { tkId :: String, tkIdFix :: Fixity }
  | TkQualified         { tkId :: String }

  -- comments
  | TkLineComment       String
  | TkBlockComment      String

  -- literals
  | TkNumber            { tkNum    :: Int    }
  | TkChar              { tkChar   :: Char   }
  | TkString            { tkString :: String }

  -- layouts
  | TkVLCurly
  | TkVRCurly
  | TkVSemicolon

  -- reserved words
  | TkModule
  | TkExports
  | TkImport
  | TkAs
  | TkOpen
  | TkReopen
  | TkClosed
  | TkWith
  | TkWhere
  | TkLet
  | TkIn
  | TkForall
  | TkExists
  | TkSelect
  | TkCase
  | TkOf
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
                TkIdentifier str _ -> showId str
                TkQualified  str   -> showId str

                TkLineComment str  -> "--" ++ str
                TkBlockComment str -> "{-" ++ str ++ "-}"

                TkNumber num       -> show num
                TkChar   chr       -> show chr
                TkString str       -> show str

                TkVLCurly          -> "{^"
                TkVRCurly          -> "^}"
                TkVSemicolon       -> "^;"

                TkModule           -> "module"
                TkExports          -> "exports"
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
                TkCase             -> "case"
                TkOf               -> "of"
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
                TkVLCurly          -> string "{^" >> raise >> line
                TkVRCurly          -> lower >> line >> string "^}"
                TkVSemicolon       -> string "^;" >> line
                _                  -> string (show tk)

prettyShowTokens :: Foldable f => f Token -> String
prettyShowTokens = runPrettyShow . pshows (string " ")
