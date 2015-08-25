{
module Language.Angler.Parser.Parser
        ( parseModule
        -- ,
        ) where

import           Language.Angler.Parser.Lexer (lexer)

import           Language.Angler.Error
import           Language.Angler.Parser.LP
import           Language.Angler.Parser.Token
import           Language.Angler.SrcLoc

import           Control.Applicative          (Alternative(..))
import           Data.Sequence                (Seq(..))
import           Prelude                      hiding ()

import           Debug.Trace                  (trace, traceShow)

}

%monad { LP } -- { >>= } { return }
%lexer { lexer } { Loc _ TkEOF }
%tokentype { (Located Token) }
%error { parseError }

-- Exported parsers
%name parseModule Module -- where
-- %name parseImport importdecl
-- %name parseStatement stmt
-- %name parseDeclaration topdecl
-- %name parseExpression exp
-- %name parsePattern pat
-- %name parseTypeSignature sigdecl
-- %name parseStmt   maybe_stmt
-- %name parseIdentifier  identifier
-- %name parseType ctype

%token
        id                      { Loc _ (TkIdentifier _) }
        qid                     { Loc _ (TkQualified _)  }
        -- nl                      { Loc _ TkNewLine        }
        '{^'                    { Loc _ TkVLCurly        }
        '^}'                    { Loc _ TkVRCurly        }
        ';'                     { Loc _ TkSemicolon      }
        'export'                { Loc _ TkExport         }
        'import'                { Loc _ TkImport         }
        'as'                    { Loc _ TkAs             }
        'where'                 { Loc _ TkWhere          }
        'forall'                { Loc _ TkForall         }
        'exists'                { Loc _ TkExists         }
        'with'                  { Loc _ TkWith           }
        'on'                    { Loc _ TkOn             }
        'is'                    { Loc _ TkIs             }
        '->'                    { Loc _ TkArrow          }
        ':'                     { Loc _ TkColon          }
        '='                     { Loc _ TkEquals         }
        ','                     { Loc _ TkComma          }
        '('                     { Loc _ TkLParen         }
        ')'                     { Loc _ TkRParen         }
        '{'                     { Loc _ TkLCurly         }
        '}'                     { Loc _ TkRCurly         }
        '_'                     { Loc _ TkUnderscore     }

%right      '->'

%%

-- for general use
Maybe(r) :: { Maybe r }
    : {- empty -}       { Nothing }
    | r                 { Just $1 }

MaybeEnd(r,e) :: { Maybe r }
    : {- empty -}       { Nothing }
    | r e               { Just $1}

List0(r) :: { Seq r } -- { Alternative l => l a }
    : {- empty -}       { empty }               -- like []
    | List1(r)          { $1    }

List1(r) :: { Seq r } -- { Alternative l => l a }
    : r                 { pure $1        }      -- like [$1]
    | List1(r) r        { $1 <|> pure $2 }      -- like $1 ++ [$2]

ListSep0(r,sep) :: { Seq r } -- { Alternative l => l a }
    : {- empty -}       { empty }               -- like []
    | ListSep1(r,sep)   { $1    }

ListSepEnd0(r,sep,e) :: { Seq r } -- { Alternative l => l a }
    : {- empty -}       { empty }               -- like []
    | ListSep1(r,sep) e { $1    }

ListSep1(r,sep) :: { Seq r } -- { Alternative l => l a }
    : r                 { pure $1        }      -- like [$1]
    | ListSep1(r,sep) sep r
                        { $1 <|> pure $3 }      -- like $1 ++ [$3]

-- identifiers
Id :: { () }
    : id                { () }

QId :: { () }
    : qid               { () }
    | Id                { () }

-- modules
Module :: { () }
    : '{^' Top ListSep1(Body,';') '^}'
                        { () }

-- export and imports
Top :: { () }
    : MaybeEnd(Export, ';')
      ListSepEnd0(Import, ';', ';')
                        { () }

    Export :: { () }
        : 'export' '(' ListSep0(QId, ',') ')'
                            { () }

    Import :: { () }
        : 'import' QId Maybe(ImportAs) Maybe(ImportSpecific)
                            { () }

            ImportAs :: { () }
                : 'as' Id       { () }

            ImportSpecific :: { () }
                : '(' ListSep0(Id, ',') ')'
                                { () }

-- declarations, definitions, behaviours, instantiation
Body :: { () }
    : Declaration       { $1 }
    | Definition        { $1 }
--    | Behaviour         { $1 }

    Declaration :: { () }
        : Id ':' Expression
                            { () }
        | Id ':' Expression 'where'
            '{^' ListSep1(WhereBody,';') '^}'
                            { () }

    Definition :: { () }
        : Id Maybe(Implicit) List0(Argument) '=' Expression
                            { () }
        | Id Maybe(Implicit) List0(Argument) '=' Expression 'where'
            '{^' ListSep1(WhereBody,';') '^}'
                            { () }

    -- for general use in 'Body'
    Expression :: { () }
        : List1(Term)       { () }

    Term :: { () }
        : QId               { () }
        | '->'              { () }
        | '(' Expression ')'
                            { () }

    WhereBody :: { () }
        : Declaration       { () }
        | Definition        { () }

    Argument :: { () }
        : Binding           { () }
        | '(' Expression ')'        -- pattern matching
                            { () }

    Binding :: { () }
        : Id                { () }
        | '_'               { () }

    Implicit :: { () }
        : '{' ListSep1(ImplicitBinding,',') '}'
                            { () }

        ImplicitBinding :: { () }
            : Id '=' Expression { () }

{

parseError :: Located Token -> LP a
parseError (Loc l tk) = throwError (Loc l (ParseError (PErr (show tk))))

}