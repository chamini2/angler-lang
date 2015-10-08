{
{-# LANGUAGE RankNTypes #-}

module Language.Angler.Parser.Parser
        ( runLexer
        , runParser
        ) where

import           Language.Angler.Parser.Lexer (evalLP, lexer, popContext, runLexer)

import           Language.Angler.AST
import           Language.Angler.Error
import           Language.Angler.Monad
import           Language.Angler.Parser.LP
import           Language.Angler.Parser.Token
import           Language.Angler.SrcLoc

import           Control.Applicative          (Alternative(..))
import           Control.Lens
import           Data.Sequence                (Seq(..))
import           Data.Foldable                (toList, msum)
import           Data.Maybe                   (isJust, fromJust)

}

%monad { LP } -- { >>= } { return }
%lexer { lexer } { Loc _ TkEOF }
%tokentype { (Located Token) }
%error { parseError }

-- Exported parser
%name parseModule Module

%token
        ident                   { Loc _ (TkIdentifier _) }
        qualf                   { Loc _ (TkQualified  _) }

        int                     { Loc _ (TkInteger _)    }
        chr                     { Loc _ (TkChar    _)    }
        str                     { Loc _ (TkString  _)    }

        '{^'                    { Loc $$ TkVLCurly       }
        '^}'                    { Loc $$ TkVRCurly       }
        '^;'                    { Loc $$ TkVSemicolon    }

        'export'                { Loc $$ TkExport        }
        'import'                { Loc $$ TkImport        }
        'as'                    { Loc $$ TkAs            }
        'open'                  { Loc $$ TkOpen          }
        'reopen'                { Loc $$ TkReopen        }
        'closed'                { Loc $$ TkClosed        }
        'with'                  { Loc $$ TkWith          }
        'let'                   { Loc $$ TkLet           }
        'in'                    { Loc $$ TkIn            }
        'where'                 { Loc $$ TkWhere         }
        'forall'                { Loc $$ TkForall        }
        'exists'                { Loc $$ TkExists        }
        'select'                { Loc $$ TkSelect        }
        -- 'on'                    { Loc $$ TkOn            }
        -- 'behaviour'             { Loc $$ TkBehaviour     }
        -- 'is'                    { Loc $$ TkIs            }
        'operator'              { Loc $$ TkOperator     }
        'prefix'                { Loc $$ TkPrefix       }
        'postfix'               { Loc $$ TkPostfix      }
        'infixL'                { Loc $$ TkInfixL       }
        'infixR'                { Loc $$ TkInfixR       }
        'infixN'                { Loc $$ TkInfixN       }

        ':'                     { Loc $$ TkColon         }
        ';'                     { Loc $$ TkSemicolon     }
        '.'                     { Loc $$ TkDot           }
        '->'                    { Loc $$ TkArrow         }
        '\ '                    { Loc $$ TkBackslash     }
        '='                     { Loc $$ TkEquals        }
        ','                     { Loc $$ TkComma         }
        -- '@'                     { Loc $$ TkAt            }
        '('                     { Loc $$ TkLParen        }
        ')'                     { Loc $$ TkRParen        }
        '{'                     { Loc $$ TkLCurly        }
        '}'                     { Loc $$ TkRCurly        }
        '_'                     { Loc $$ TkUnderscore    }

%%

-- for general use
Maybe(r) :: { Maybe r }
    : {- empty -}   { Nothing }
    | r             { Just $1 }

MaybeEnd(r,e) :: { Maybe r }
    : {- empty -}   { Nothing }
    | r e           { Just $1 }

List0(r) :: { Seq r }
    : {- empty -}   { empty }               -- like []
    | List1(r)      { $1    }

List1(r) :: { Seq r }
    : r             { pure $1  }            -- like [$1]
    | List1(r) r    { $1 |> $2 }            -- like $1 ++ [$2]

ListSep0(r,sep) :: { Seq r }
    : {- empty -}   { empty }               -- like []
    | ListSep1(r,sep)
                    { $1    }

ListSepEnd0(r,sep,e) :: { Seq r }
    : {- empty -}   { empty }               -- like []
    | ListSep1(r,sep) e
                    { $1    }

ListSep1(r,sep) :: { Seq r }
    : r             { pure $1  }            -- like [$1]
    | ListSep1(r,sep) sep r
                    { $1 |> $3 }            -- like $1 ++ [$3]

--------------------------------------------------------------------------------
-- identifiers
Id :: { IdentifierSpan }
    : ident
                    { Identifier ($1^.loc_insd.to tkId) ($1^.loc_span) }

QId :: { IdentifierSpan }
    : qualf         { Identifier ($1^.loc_insd.to tkId) ($1^.loc_span) }
    | Id            { $1 }

DotId :: { IdentifierSpan }
    : '.'           { Identifier "."  $1 }

ArrowId :: { IdentifierSpan }
    : '->'          { Identifier "->" $1 }

EqualsId :: { IdentifierSpan }
    : '='           { Identifier "="  $1 }

----------------------------------------
-- module
Module :: { ModuleSpan }
    : '{^' Top Body '^}'
                    { Module "" (fst $2) (snd $2) $3 (srcSpanSpan $1 $4) }

----------------------------------------
-- export and imports
Top :: { (Maybe (Seq IdentifierSpan), Seq ImportSpan) }
    : MaybeEnd(Export, '^;') ListSepEnd0(Import, '^;', '^;')
                    { ($1, $2) }

    Export :: { Seq IdentifierSpan }
        : 'export' '(' ListSep0(Id, ',') ')'
                        { $3 }

    Import :: { ImportSpan }
        : 'import' QId Maybe(ImportAs) Maybe(ImportSpecific)
                        { Import ($2^.idn_str) $3 (fmap fst $4)
                            (srcSpanSpan $1
                                (fromJust (msum
                                    -- get the farthest span
                                    [ fmap (^.idn_annot) $3
                                    , fmap snd           $4
                                    , Just ($2^.idn_annot)] )))
                        }

        ImportAs :: { IdentifierSpan }
            : 'as' QId      { $2 }

            ----------------------------
            -- errors
            | 'as' {- empty -}
                            {% throwPError (PErrExpectingIn "identifier" "as") $1 }

        ImportSpecific :: { (Seq IdentifierSpan, SrcSpan) }
            : '(' ListSep0(Id, ',') ')'
                            { ($2, srcSpanSpan $1 $3) }

----------------------------------------
-- declarations, definitions
Body :: { BodySpan }
    : ListSep1(BodyStmt, '^;')
                    { $1 }

    CloseBrace :: { () }
        : '^}'          { () }
        | error         {% popContext }

    BodyStmt :: { BodyStmtSpan }
        -- open type definition
            -- open Currency with
            --     USD : Nat -> Currency
        : 'open' Id ':' ExpressionWhere Maybe(Constructors)
                        { OpenType $2 $4 ($5 & _Just %~ fst)
                            (srcSpanSpan $1 (maybe ($4^.whre_annot) snd $5)) }

        -- reopening type definition
            -- reopen Currency with
            --     EUR : Nat -> Currency
        | 'reopen' QId Constructors
                        { ReopenType $2 (fst $3)
                            (srcSpanSpan $1 (snd $3)) }

        -- closed type definition
            -- closed Nat : Type with
            --     Z : Nat
            --     S : Nat -> Nat
        | 'closed' Id ':' ExpressionWhere Constructors
                        { ClosedType $2 $4 (fst $5)
                            (srcSpanSpan $1 (snd $5)) }

        -- function declaration
            -- _$_ : forall a:Type, b:Type . (a -> b) -> a -> b
        | Id ':' ExpressionWhere
                        { FunctionDecl $1 $3
                            (srcSpanSpan ($1^.idn_annot) ($3^.whre_annot)) }

        -- function definition
            -- f $ x = f x
        | List1(Argument(FunId)) '=' ExpressionWhere
                        { FunctionDef $1 $3
                            (srcSpanSpan ($1^?!_head.arg_annot) ($3^.whre_annot)) }
        | 'operator' Id Fixity Maybe(LitInt)
                        { OperatorDef $2 $3 ($4^?_Just.lit_int)
                            (srcSpanSpan $1 (maybe ($3^.fix_annot) (^.lit_annot) $4)) }

        -- behaviour namespace
        -- | BehaviourNamespace
        -- behaviour declaration
        -- | Behaviour
        -- instance definition
        -- | Instance

        --------------------------------
        -- errors
        | 'open' {- empty -}
                        {% throwPError (PErrExpectingIn "identifier" "open") $1 }
        | 'open' Id {- empty -}
                        {% throwPError (PErrExpectingIn "type signature" "open")
                            (srcSpanSpan $1 ($2^.idn_annot)) }

        | 'reopen' {- empty -}
                        {% throwPError (PErrExpectingIn "identifier" "reopen") $1 }
        | 'reopen' QId
                        {% throwPError (PErrExpectingIn "constructors" "reopen")
                            (srcSpanSpan $1 ($2^.idn_annot)) }

        | 'closed' {- empty -}
                        {% throwPError (PErrExpectingIn "identifier" "closed") $1 }
        | 'closed' Id {- empty -}
                        {% throwPError (PErrExpectingIn "type signature" "closed")
                            (srcSpanSpan $1 ($2^.idn_annot)) }
        | 'closed' Id ':' ExpressionWhere {- empty -}
                        {% throwPError (PErrExpectingIn "constructors" "closed")
                            (srcSpanSpan $1 ($4^.whre_annot)) }
        | Id ':' {- empty -}
                        {% throwPError (PErrExpectingIn "type signature" "function declaration")
                            (srcSpanSpan ($1^.idn_annot) $2) }
        | {- empty -} '=' ExpressionWhere
                        {% throwPError (PErrExpectingIn "identifier" "function definition")
                            (srcSpanSpan $1 ($2^.whre_annot)) }
        | List1(Argument(FunId)) '='
                        {% throwPError (PErrExpectingIn "expression" "function definition")
                            (srcSpanSpan ($1^?!_head.arg_annot) $2) }

    ----------------------------------------

        Where :: { (BodySpan, SrcSpan) }
            : 'where' '{^' Body '^}'    -- change Body to only functions
                            { ($3, srcSpanSpan $1 $4) }

        Constructors :: { (Seq TypeBindSpan, SrcSpan) }
            : 'with'
                '{^' ListSep1(TypeBindWhere, '^;') '^}'
                            { ($3, srcSpanSpan $1 $4) }

        Fixity :: { FixitySpan }
            : 'closed'      { Closedfix        $1 }
            | 'prefix'      { Prefix           $1 }
            | 'postfix'     { Postfix          $1 }
            | 'infixL'      { Infix LeftAssoc  $1 }
            | 'infixR'      { Infix RightAssoc $1 }
            | 'infixN'      { Infix NonAssoc   $1 }

        Argument(argid) :: { ArgumentSpan }
            : '_'           { DontCare $1 }
            | argid         { Binding $1 ($1^.idn_annot) }
            | '(' List1(Argument(ExpId)) ')'
                            { ParenthesizedBinding $2 (srcSpanSpan $1 $3) }

        FunId :: { IdentifierSpan }
            : QId           { $1 }
            | DotId         { $1 }
            | ArrowId       { $1 }

        TypeBindWhere :: { TypeBindSpan }
            : TypeBind Maybe(Where)
                            { $1 & typ_type %~ \whre ->
                                getWhere $2 (whre^.whre_insd) exp_annot }

        TypeBind :: { TypeBindSpan }
            : TypeBind_(ExpId)
                            { $1 }

        TypeBind_(expid) :: { TypeBindSpan }
            : Id ':' Expression_(expid)
                            { TypeBind $1 (Where $3 Nothing ($3^.exp_annot))
                                (srcSpanSpan ($1^.idn_annot) ($3^.exp_annot)) }

    ----------------------------------------

        ExpressionWhere :: { ExprWhereSpan }
            : Expression Maybe(Where)
                            { getWhere $2 $1 exp_annot }

        Expression :: { ExpressionSpan }
            : Expression_(ExpId)
                            { $1 }

            Expression_(expid) :: { ExpressionSpan }
                : ExpressionList_(expid)
                                -- { let s l r = srcSpanSpan (l^.exp_annot) (r^.exp_annot)
                                --   in foldl1 (\l r -> Apply l r (s l r)) $1 }
                                { if length $1 == 1
                                    then $1^?!_head
                                    else Apply $1
                                            (srcSpanSpan ($1^?!_head.exp_annot)
                                                         ($1^?!_last.exp_annot)) }

            ExpId :: { IdentifierSpan }
                : QId           { $1 }
                | DotId         { $1 }
                | ArrowId       { $1 }
                | EqualsId      { $1 }

            ExpressionList_(expid) :: { Seq ExpressionSpan }
                : '\ ' List1(Argument(LambdaId)) '->' Expression_(expid)
                                { let s a e = srcSpanSpan (a^.arg_annot) (e^.exp_annot)
                                  in pure $ foldr (\a e -> Lambda a e (s a e)) $4 $2 }
                | 'let' '{^' Body CloseBrace 'in' Expression_(expid)
                                { pure $ Let $3 $6 (srcSpanSpan $1 ($6^.exp_annot)) }
                | 'forall' ListSep1(TypeBind_(ForallId), ',') '.' Expression_(expid)
                                { pure $ Forall $2 $4
                                    (srcSpanSpan $1 ($4^.exp_annot)) }
                | 'exists' TypeBind ';' Expression_(expid)
                                { pure $ Exists $2 $4
                                    (srcSpanSpan $1 $3) }
                | 'select' TypeBind_(expid)
                                { pure $ Select $2
                                    (srcSpanSpan $1 ($2^.typ_annot)) }
                | Term(expid) ExpressionList_(expid)
                                { $1 <| $2 }
                | Term(expid)
                                { pure $1 }

                ------------------------
                -- errors
                | '\ ' {- empty -} '->'
                                {% throwPError (PErrNoArgumentsIn "lambda")
                                    (srcSpanSpan $1 $2) }
                | '\ ' List1(Argument(LambdaId)) '->' {- empty -}
                                {% throwPError (PErrNoExpressionIn "lambda")
                                    (srcSpanSpan $1 $3) }
                | 'let' '{^' {- empty -} CloseBrace 'in'
                                {% throwPError (PErrEmptyLayoutAfter "let")
                                    (srcSpanSpan $1 $4) }
                | 'let' '{^' Body CloseBrace 'in' {- empty -}
                                {% throwPError (PErrNoExpressionIn "let-in")
                                    (srcSpanSpan $1 $5) }
                | 'forall' {- empty -} '.'
                                {% throwPError (PErrNoVariablesIn "forall")
                                    (srcSpanSpan $1 $2) }
                | 'forall' ListSep1(TypeBind_(ForallId), ',') '.' {- empty -}
                                {% throwPError (PErrNoExpressionIn "forall")
                                    (srcSpanSpan $1 $3) }
                | 'exists' {- empty -} ';'
                                {% throwPError (PErrNoVariableIn "exists")
                                    (srcSpanSpan $1 $2) }
                | 'exists' TypeBind ';' {- empty -}
                                {% throwPError (PErrNoExpressionIn "exists")
                                        (srcSpanSpan $1 $3) }
                | 'select' Expression_(expid)
                                {% throwPError (PErrNoBindIn "select")
                                        (srcSpanSpan $1 ($2^.exp_annot))}

                LambdaId :: { IdentifierSpan }
                    : QId           { $1 }
                    | DotId         { $1 }
                    | EqualsId      { $1 }

                ForallId :: { IdentifierSpan }
                    : QId           { $1 }
                    | ArrowId       { $1 }
                    | EqualsId      { $1 }

                Term(expid) :: { ExpressionSpan }
                    : expid         { Var ($1^.idn_str) ($1^.idn_annot) }
                    | Literal       { Lit $1 ($1^.lit_annot) }
                    | '{' ListSep1(ImplicitBinding, ',') '}'
                                    { ImplicitExpr $2 (srcSpanSpan $1 $3) }
                    | '(' Expression ')'
                                    { $2 & exp_annot .~ srcSpanSpan $1 $3 }

                    Literal :: { LiteralSpan }
                        : LitInt        { $1 }
                        | LitChar       { $1 }
                        | LitString     { $1 }

                    LitInt :: { LiteralSpan }
                        : int           { LitInt    ($1^.loc_insd.to tkInt)
                                            ($1^.loc_span) }

                    LitChar :: { LiteralSpan }
                        : chr           { LitChar   ($1^.loc_insd.to tkChar)
                                            ($1^.loc_span) }

                    LitString :: { LiteralSpan }
                        : str           { LitString ($1^.loc_insd.to tkString)
                                            ($1^.loc_span) }

                    ImplicitBinding :: { ImplicitBindingSpan }
                        : Id '=' Expression
                                        { ImplicitBind $1 $3
                                            (srcSpanSpan ($1^.idn_annot)
                                                         ($3^.exp_annot)) }

{

parseError :: Located Token -> LP a
parseError (Loc l tk) = case tk of
        TkVLCurly    -> lexer parseError
        TkVRCurly    -> lexer parseError
        TkVSemicolon -> lexer parseError
        _            -> (throwError . Loc l . ParseError . PErr . show) tk

throwPError :: ParseError -> SrcSpan -> LP a
throwPError err = throwError . flip Loc (ParseError err)

getWhere :: Maybe (BodySpan, SrcSpan) -> f SrcSpan -> (Lens' (f SrcSpan) SrcSpan) -> WhereSpan f
getWhere mwhre e elns = case mwhre of
        Just (bdy, spn) -> Where e (Just bdy) (srcSpanSpan (e^.elns) spn)
        Nothing         -> Where e Nothing    (e^.elns)

runParser :: String -> SrcLoc -> Either (Located Error) (ModuleSpan, [Located Warning])
runParser input loc = evalLP input loc parseModule

}
