{
module Language.Angler.Parser.Parser
        ( parseModule
        -- ,
        ) where

import           Language.Angler.Parser.Lexer (lexer)

import           Language.Angler.AST
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


-- lambda expressions in the 'Term' rule currently generates 9 shift/reduce
-- conflicts, which are resolved as shift
-- %expect 9

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
        ident                   { Loc _ (TkIdentifier _) }
        -- imprt                   { Loc _ (TkImportPath _) }
        qualf                   { Loc _ (TkQualified  _) }

        int                     { Loc _ (TkInteger _)    }
        chr                     { Loc _ (TkChar    _)    }
        str                     { Loc _ (TkString  _)    }

        '{^'                    { Loc _ TkVLCurly        }
        '^}'                    { Loc _ TkVRCurly        }
        '^;'                    { Loc _ TkVSemicolon     }

        'export'                { Loc _ TkExport         }
        'import'                { Loc _ TkImport         }
        'as'                    { Loc _ TkAs             }
        'closed'                { Loc _ TkClosed         }
        'open'                  { Loc _ TkOpen           }
        'where'                 { Loc _ TkWhere          }
        'forall'                { Loc _ TkForall         }
        'exists'                { Loc _ TkExists         }
        'with'                  { Loc _ TkWith           }
        -- 'on'                    { Loc _ TkOn             }
        -- 'is'                    { Loc _ TkIs             }

        ':'                     { Loc _ TkColon          }
        ';'                     { Loc _ TkSemicolon      }
        '.'                     { Loc _ TkDot            }
        '->'                    { Loc _ TkArrow          }
        '\ '                    { Loc _ TkBackslash      }
        '='                     { Loc _ TkEquals         }
        ','                     { Loc _ TkComma          }
        '('                     { Loc _ TkLParen         }
        ')'                     { Loc _ TkRParen         }
        '{'                     { Loc _ TkLCurly         }
        '}'                     { Loc _ TkRCurly         }
        '_'                     { Loc _ TkUnderscore     }

%%

-- for general use
Maybe(r) :: { Maybe r }
    : {- empty -}       { Nothing }
    | r                 { Just $1 }

MaybeEnd(r,e) :: { Maybe r }
    : {- empty -}       { Nothing }
    | r e               { Just $1 }

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

--------------------------------------------------------------------------------
-- identifiers
Id :: { Identifier }
    : ident             { let Loc l (TkIdentifier str) = $1 in Loc l str }

QId :: { Qualified }
    : qualf             { let Loc l (TkQualified str) = $1 in Loc l str }
    | Id                { $1 }
    -- | ImportPath        { $1 }

-- ImportPath :: { Qualified }
--     : imprt             { let Loc l (TkImportPath str) = $1 in Loc l str }

----------------------------------------
-- modules
Module :: { Module }
    : '{^' Top ListSep1(Body,'^;') '^}'
                        { Module (fst $2) (snd $2) $3 }

----------------------------------------
-- export and imports
Top :: { (Maybe (Seq Identifier), Seq (Located Import)) }
    : MaybeEnd(Export, '^;')
      ListSepEnd0(Import, '^;', '^;')
                        { ($1, $2) }

    Export :: { Seq Identifier }
        : 'export' '(' ListSep0(Id, ',') ')'
                            { $3 }

    Import :: { Located Import }
        : 'import' QId ImportOptions
                            { Loc (srcLocatedSpan $1 $2)
                                (Import (unlocate $2) (fst $3) (snd $3)) }

            ImportOptions :: { (String, Maybe (Seq Identifier)) }
                : Maybe(ImportSpecific)
                                { ("", $1) }
                | 'as' '{^' QId Maybe(ImportSpecific) '^}'  -- 'as' produces a
                                { (unlocate $3, $4) }       -- layout because is
                                                            -- used in datas

                ImportSpecific :: { () }
                    : '(' ListSep0(Id, ',') ')'
                                    { () }

    -- if we stop producing a layout after 'as'
    -- Import : 'import' QId Maybe(ImportAs) Maybe(ImportSpecific) {}
    --         ImportAs : 'as' QId {}
    --         ImportSpecific : '(' ListSep0(Id, ',') ')' {}

----------------------------------------
-- declarations, definitions
-- Body :: { () }
    -- : BodyStmt
                        -- { () }

    Body :: { () }
        : Declaration       { $1 }
        | Function          { $1 }

        Declaration :: { () }
            :          Type { () }
            | 'closed' Type 'as'
                '{^' ListSep1(Type, '^;') '^}'
                            { () }

        Type :: { () }
            : Id ':' Expression Maybe(Where)
                                { () }

        Function :: { () }
            : List1(Argument) Maybe(Implicit) '=' Expression Maybe(Where)
                                { () }

            Implicit :: { () }
                : '{' ListSep1(ImplicitBinding,',') '}'
                                    { () }

                ImplicitBinding :: { () }
                    : Id '=' Expression { () }

            Argument :: { () }
                : Binding           { () }
                | '(' Expression ')'        -- pattern matching
                                    { () }

                Binding :: { () }
                    : Id                { () }
                    | '_'               { () }

        -- for general use in 'Body'
        Expression :: { () }
            : List1(Term)       { () }

            Term :: { () }
                : QId               { () }
                | '.'               { () }
                | '='               { () }
                | '->'              { () }
                | '\ ' List1(Argument) '->'
                                    { () }
                | '(' Expression ')'
                                    { () }
                | 'forall' '(' ListSep1(Type,',') ')' '.'
                                    { () }
                | 'exists' '(' Type ';' Expression ')'
                                    { () }
                | 'with' '(' Type ')'
                                    { () }
                | Implicit          { () }

    Where :: { () }
        : 'where' '{^' ListSep1(Body, '^;') '^}'
                            { () }

{

parseError :: Located Token -> LP a
parseError (Loc l tk) = throwError (Loc l (ParseError (PErr (show tk))))

}