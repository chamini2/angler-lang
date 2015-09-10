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
import           Control.Lens
import           Data.Sequence                (Seq(..))
import           Data.Foldable                (toList)

}

%monad { LP } -- { >>= } { return }
%lexer { lexer } { Loc _ TkEOF }
%tokentype { (Located Token) }
%error { parseError }

-- Exported parsers
%name parseModule Module
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
        'reopen'                { Loc _ TkReopen         }
        'where'                 { Loc _ TkWhere          }
        'forall'                { Loc _ TkForall         }
        'exists'                { Loc _ TkExists         }
        'with'                  { Loc _ TkWith           }
        -- 'on'                    { Loc _ TkOn             }
        -- 'behaviour'             { Loc _ TkBehaviour      }
        -- 'is'                    { Loc _ TkIs             }

        ':'                     { Loc _ TkColon          }
        ';'                     { Loc _ TkSemicolon      }
        '.'                     { Loc _ TkDot            }
        '->'                    { Loc _ TkArrow          }
        '\ '                    { Loc _ TkBackslash      }
        '='                     { Loc _ TkEquals         }
        ','                     { Loc _ TkComma          }
        -- '@'                     { Loc _ TkAt             }
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

List0(r)
    : {- empty -}       { empty }               -- like []
    | List1(r)          { $1    }

List1(r)
    : r                 { pure $1  }            -- like [$1]
    | List1(r) r        { $1 |> $2 }            -- like $1 ++ [$2]

ListSep0(r,sep)
    : {- empty -}       { empty }               -- like []
    | ListSep1(r,sep)   { $1    }

ListSepEnd0(r,sep,e)
    : {- empty -}       { empty }               -- like []
    | ListSep1(r,sep) e { $1    }

ListSep1(r,sep)
    : r                 { pure $1  }            -- like [$1]
    | ListSep1(r,sep) sep r
                        { $1 |> $3 }            -- like $1 ++ [$3]

--------------------------------------------------------------------------------
-- identifiers
Id :: { IdentifierSpan }
    : ident             { Identifier ($1^.loc_insd.to tkId) ($1^.loc_span) }

QId :: { IdentifierSpan }
    : qualf             { Identifier (tkId ($1^.loc_insd)) ($1^.loc_span) }
    | Id                { $1 }

ReservedSymbols :: { IdentifierSpan }
    : ':'               { Identifier ":"  ($1^.loc_span) }
    | '.'               { Identifier "."  ($1^.loc_span) }
    | '->'              { Identifier "->" ($1^.loc_span) }
    -- | '\ '              { Identifier "\\" ($1^.loc_span) }
    | '='               { Identifier "="  ($1^.loc_span) }
    -- | ','               { Identifier ","  ($1^.loc_span) }

----------------------------------------
-- modules
Module :: { ModuleSpan }
    : '{^' Top Body '^}'
                        { Module (fst $2) (snd $2) $3
                            (srcLocatedSpan $1 $4) }

----------------------------------------
-- export and imports
Top :: { (Maybe (Seq IdentifierSpan), Seq ImportSpan) }
    : MaybeEnd(Export, '^;')
        ListSepEnd0(Import, '^;', '^;')
                        { ($1, $2) }

    Export :: { Seq IdentifierSpan }
        : 'export' '(' ListSep0(Id, ',') ')'
                            { $3 }

    Import :: { ImportSpan }
        : 'import' QId ImportOptions
                            { Import ($2^.idn_str) (fst $3) (snd $3)
                                (srcSpanSpan ($1^.loc_span) ($2^.idn_annot)) }

            ImportOptions :: { (Maybe IdentifierSpan, Maybe (Seq IdentifierSpan)) }
                : Maybe(ImportSpecific)
                                { (Nothing, $1) }
                | 'as' '{^' QId Maybe(ImportSpecific) '^}'  -- 'as' produces a
                                { (Just $3, $4) }           -- layout because is
                                                            -- used in datas

                ImportSpecific :: { Seq IdentifierSpan }
                    : '(' ListSep0(Id, ',') ')'
                                    { $2 }

    -- if we stop producing a layout after 'as'
    -- Import : 'import' QId Maybe(ImportAs) Maybe(ImportSpecific) {}
    --         ImportAs : 'as' QId {}
    --         ImportSpecific : '(' ListSep0(Id, ',') ')' {}

----------------------------------------
-- declarations, definitions
Body :: { BodySpan }
    : ListSep1(BodyStmt, '^;')
                        { $1 }

    BodyStmt :: { BodyStmtSpan }
        : Declaration       { $1 }
        | Definition        { $1 }

        Declaration :: { BodyStmtSpan }
            :          Type
                            { FunctionDecl ($1^.typ_id) ($1^.typ_type)
                                ($1^.typ_annot) }
            | 'open'   Type Maybe(Constructors)
                            { OpenType ($2^.typ_id) ($2^.typ_type) (fmap fst $3)
                                (srcSpanSpan ($1^.loc_span)
                                             (maybe ($2^.typ_annot) snd $3)) }
            | 'reopen' QId  Constructors
                            { ReopenType $2 (fst $3)
                                (srcSpanSpan ($1^.loc_span) (snd $3)) }
            | 'closed' Type Constructors
                            { ClosedType ($2^.typ_id) ($2^.typ_type) (fst $3)
                                (srcSpanSpan ($1^.loc_span) (snd $3)) }

            Constructors :: { (Seq (TypeDeclSpan), SrcSpan) }
                : 'as'
                    '{^' ListSep1(Type, '^;') '^}'
                                { ($3, srcLocatedSpan $1 $4) }

        Type :: { TypeDeclSpan }
            : Id ':' Expression Maybe(Where)
                                { TypeDecl $1
                                    (Where (maybe empty fst $4) $3
                                        (maybe SrcSpanNoInfo snd $4))
                                    (srcSpanSpan ($1^.idn_annot) ($3^.exp_annot)) }

        Definition :: { BodyStmtSpan }
            : List1(Argument) '=' Expression Maybe(Where)
                                { FunctionDef $1
                                    (Where (maybe empty fst $4) $3
                                        (maybe SrcSpanNoInfo snd $4))
                                    (srcSpanSpan ($1^?!_head.arg_annot) ($3^.exp_annot)) }

            Argument :: { ArgumentSpan }
                : '_'               { DontCare ($1^.loc_span) }
                | '(' List1(ArgExpId) ')'
                                    { ParenthesizedBinding $2
                                        (srcLocatedSpan $1 $3) }
                | QId               { Binding $1 ($1^.idn_annot) }

                ArgExpId :: { ArgumentSpan }
                    : '_'               { DontCare ($1^.loc_span) }
                    | '(' List1(ArgExpId) ')'
                                        { ParenthesizedBinding $2
                                            (srcLocatedSpan $1 $3) }
                    | ExpId             { Binding $1 ($1^.idn_annot) }

        -- for general use in 'Body'
        Expression :: { ExpressionSpan }
            : List1(Term)       { if length $1 == 1
                                    then head (toList $1)
                                    else Application $1
                                        (srcSpanSpan ($1 ^?! _head.exp_annot)
                                                     ($1 ^?! _last.exp_annot))
                                }

            Term :: { ExpressionSpan }
                : ExpId             { Var ($1^.idn_str) ($1^.idn_annot) }
                | '\ ' List1(Argument) '->'
                                    { Lambda $2
                                        (srcLocatedSpan $1 $3) }
                | '(' Expression ')'
                                    { $2 & exp_annot .~ (srcLocatedSpan $1 $3) }
                | 'forall' '(' ListSep1(Type,',') ')'
                                    { Forall $3
                                        (srcLocatedSpan $1 $4) }
                | 'exists' '(' Type ';' Expression ')'
                                    { Exists $3 $5
                                        (srcLocatedSpan $1 $6) }
                | 'with' '(' Type ')'
                                    { With $3
                                        (srcLocatedSpan $1 $4) }
                | '{' ListSep1(ImplicitBinding,',') '}'
                                    { ImplicitExpr $2 (srcLocatedSpan $1 $3) }

                ExpId :: { IdentifierSpan }
                    : QId               { $1 }
                    | ':'               { Identifier ":"  ($1^.loc_span) }
                    | '->'              { Identifier "->" ($1^.loc_span) }
                    | '='               { Identifier "="  ($1^.loc_span) }

                ImplicitBinding :: { ImplicitBindingSpan }
                    : Id '=' Expression { ImplicitBind $1 $3
                                            (srcSpanSpan ($1^.idn_annot)
                                                         ($3^.exp_annot)) }

        Where :: { (BodySpan, SrcSpan) }
            : 'where' '{^' Body '^}'
                                { ($3, srcLocatedSpan $1 $4) }

{

parseError :: Located Token -> LP a
-- parseError (Loc l tk) = throwError (Loc l (ParseError (PErr (show tk))))
parseError (Loc l tk) = case tk of
        -- TkVLCurly    -> lexer parseError
        -- TkVRCurly    -> lexer parseError
        -- TkVSemicolon -> lexer parseError
        _ -> throwError (Loc l (ParseError (PErr (show tk))))

}