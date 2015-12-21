{
{-# LANGUAGE RankNTypes #-}

module Language.Angler.Parser.Parser
        ( lexProgram
        , parseProgram
        ) where

import           Language.Angler.Parser.Lexer (evalLP, lexer, popContext, lexProgram)

import           Language.Angler.Program
import           Language.Angler.Error
import           Language.Angler.Monad
import           Language.Angler.Parser.LP
import           Language.Angler.Parser.Token hiding (Fixity(..))
import qualified Language.Angler.Parser.Token as Tok
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
        ident                   { Loc _ (TkIdentifier _ _) }
        qualf                   { Loc _ (TkQualified  _) }

        int                     { Loc _ (TkNumber _)     }
        chr                     { Loc _ (TkChar   _)     }
        str                     { Loc _ (TkString _)     }

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
        'where'                 { Loc $$ TkWhere         }
        'let'                   { Loc $$ TkLet           }
        'in'                    { Loc $$ TkIn            }
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
        -- ';'                     { Loc $$ TkSemicolon     }
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
    : IdFix         { fst $1 }

QId :: { IdentifierSpan }
    : qualf         { Identifier ($1^.loc_insd.to tkId) ($1^.loc_span) }
    | Id            { $1 }

DotId :: { IdentifierSpan }
    : DotIdFix      { fst $1 }

ArrowId :: { IdentifierSpan }
    : ArrowIdFix    { fst $1 }

EqualsId :: { IdentifierSpan }
    : EqualsIdFix   { fst $1 }

----------------------------------------
-- identifiers with fixity
IdFix :: { (IdentifierSpan, Tok.Fixity) }
    : ident         { (Identifier ($1^.loc_insd.to tkId) ($1^.loc_span),
                       $1^.loc_insd.to tkIdFix) }

DotIdFix :: { (IdentifierSpan, Tok.Fixity) }
    : '.'           { (Identifier "."  $1, Tok.Nofix) }

ArrowIdFix :: { (IdentifierSpan, Tok.Fixity) }
    : '->'          { (Identifier "->" $1, Tok.Nofix) }

EqualsIdFix :: { (IdentifierSpan, Tok.Fixity) }
    : '='           { (Identifier "="  $1, Tok.Nofix) }

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
                                    , Just ($2^.idn_annot) ]))) }

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
                    { Body $1 }

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
        | Argument(FunId) '=' ExpressionWhere
                        { FunctionDef $1 $3
                            (srcSpanSpan ($1^.arg_annot) ($3^.whre_annot)) }

        -- operator definition
        | OperatorDefPrec('prefix')
                        {% handleOperatorDef ($1 Tok.Prefix    Prefix)             }
        | OperatorDefPrec('postfix')
                        {% handleOperatorDef ($1 Tok.Postfix   Postfix)            }
        | OperatorDefPrec('infixL')
                        {% handleOperatorDef ($1 Tok.Infix     (Infix LeftAssoc))  }
        | OperatorDefPrec('infixR')
                        {% handleOperatorDef ($1 Tok.Infix     (Infix RightAssoc)) }
        | OperatorDefPrec('infixN')
                        {% handleOperatorDef ($1 Tok.Infix     (Infix NonAssoc))   }
        | OperatorDef('closed')
                        {% handleOperatorDef ($1 Tok.Closedfix Closedfix)          }

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
        | Argument(FunId) '='
                        {% throwPError (PErrExpectingIn "expression" "function definition")
                            (srcSpanSpan ($1^.arg_annot) $2) }

    ----------------------------------------

        Where :: { (BodySpan, SrcSpan) }
            : 'where' '{^' Body '^}'    -- change Body to only functions
                            { ($3, srcSpanSpan $1 $4) }

        Constructors :: { (Seq TypeBindSpan, SrcSpan) }
            : 'with'
                '{^' ListSep1(TypeBindWhere, '^;') '^}'
                            { ($3, srcSpanSpan $1 $4) }

        OperatorDef(fixity) :: { Tok.Fixity -> (SrcSpan -> FixitySpan) ->
                                 Either (ParseError, SrcSpan) BodyStmtSpan }
           : 'operator' IdFix fixity
                            { defineOperator $2 $3 (srcSpanSpan $1 $3) }

        OperatorDefPrec(fixity) :: { Tok.Fixity -> (Int -> SrcSpan -> FixitySpan) ->
                                 Either (ParseError, SrcSpan) BodyStmtSpan }
           : OperatorDef(fixity) LitNat
                            { \tokFix fix -> $1 tokFix (fix ($2^?!lit_nat)) }

        Argument(argid) :: { ArgumentSpan }
            : List1(Argument_(argid))
                            { if length $1 == 1
                                then $1^?!_head
                                else ApplyBinding $1
                                        (srcSpanSpan ($1^?!_head.arg_annot)
                                                     ($1^?!_last.arg_annot)) }

        Argument_(argid) :: { ArgumentSpan }
            : '_'           { DontCare $1 }
            | argid         { VarBinding ($1^.idn_str) ($1^.idn_annot) }
            | '(' Argument(ExpId) ')'
                            { $2 & arg_annot .~ (srcSpanSpan $1 $3) }

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
                : '\ ' List1(Argument_(LambdaId)) '->' Expression_(expid)
                                {% throwPError (PErr "λ-function not supported")
                                        (srcSpanSpan $1 ($4^.exp_annot)) }
                                -- { let s a e = srcSpanSpan (a^.arg_annot) (e^.exp_annot)
                                --   in pure $ foldr (\a e -> Lambda a e (s a e)) $4 $2 }
                | 'let' '{^' Body CloseBrace 'in' Expression_(expid)
                                { pure $ Let $3 $6 (srcSpanSpan $1 ($6^.exp_annot)) }
                | 'forall' ListSep1(TypeBind_(QuantifierId), ',') '.' Expression_(expid)
                                { pure $ Forall $2 $4
                                    (srcSpanSpan $1 ($4^.exp_annot)) }
                | 'exists' TypeBind_(QuantifierId) '.' Expression_(expid)
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
                                {% throwPError (PErrNoIn "arguments" "lambda")
                                    (srcSpanSpan $1 $2) }
                | '\ ' List1(Argument_(LambdaId)) '->' {- empty -}
                                {% throwPError (PErrNoIn "expression" "lambda")
                                    (srcSpanSpan $1 $3) }
                | 'let' '{^' {- empty -} CloseBrace 'in'
                                {% throwPError (PErrEmptyLayoutAfter "let")
                                    (srcSpanSpan $1 $4) }
                | 'let' '{^' Body CloseBrace 'in' {- empty -}
                                {% throwPError (PErrNoIn "expression" "let-in")
                                    (srcSpanSpan $1 $5) }
                | 'forall' {- empty -} '.'
                                {% throwPError (PErrNoIn "type binds" "forall")
                                    (srcSpanSpan $1 $2) }
                | 'forall' ListSep1(TypeBind_(QuantifierId), ',') '.' {- empty -}
                                {% throwPError (PErrNoIn "expression" "forall")
                                    (srcSpanSpan $1 $3) }
                | 'exists' {- empty -} '.'
                                {% throwPError (PErrNoIn "type bind" "exists")
                                    (srcSpanSpan $1 $2) }
                | 'exists' TypeBind_(QuantifierId) '.' {- empty -}
                                {% throwPError (PErrNoIn "expression" "exists")
                                        (srcSpanSpan $1 $3) }
                | 'select' Expression_(expid)
                                {% throwPError (PErrNoIn "binding identifier" "select")
                                        (srcSpanSpan $1 ($2^.exp_annot)) }

                LambdaId :: { IdentifierSpan }
                    : QId           { $1 }
                    | DotId         { $1 }
                    | EqualsId      { $1 }

                QuantifierId :: { IdentifierSpan }
                    : QId           { $1 }
                    | ArrowId       { $1 }
                    | EqualsId      { $1 }

                Term(expid) :: { ExpressionSpan }
                    : expid         { Var ($1^.idn_str) ($1^.idn_annot) }
                    | Literal       { Lit $1 ($1^.lit_annot) }
                    | '{' ListSep1(ImplicitBinding, ',') '}'
                                    {% throwPError (PErr "implicit apply not supported")
                                                (srcSpanSpan $1 $3) }
                                --     { ImplicitExpr $2 (srcSpanSpan $1 $3) }
                    | '(' Expression ')'
                                    { $2 & exp_annot .~ srcSpanSpan $1 $3 }

                    Literal :: { LiteralSpan }
                        : LitNat        { $1 }
                        | LitChar       { $1 }
                        | LitString     { $1 }

                    LitNat :: { LiteralSpan }
                        : int           { LitNat    ($1^.loc_insd.to tkNum)
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
        _            -> (throwError . Loc l . ParseError . PErrUnexpectedToken . show) tk

throwPError :: ParseError -> SrcSpan -> LP a
throwPError err = throwError . flip Loc (ParseError err)

defineOperator :: (IdentifierSpan, Tok.Fixity) -> SrcSpan -> SrcSpan
               -> Tok.Fixity -> (SrcSpan -> FixitySpan)
               -> Either (ParseError, SrcSpan) BodyStmtSpan
defineOperator idn fixSpn spn tokFix fixConstr = if snd idn == tokFix
        then Right (OperatorDef (fst idn) (fixConstr fixSpn) spn)
        else Left (PErrUnexpectedAfter "fixity" (show (snd idn) ++ " identifier"), spn)

handleOperatorDef :: Either (ParseError, SrcSpan) BodyStmtSpan -> LP BodyStmtSpan
handleOperatorDef = either (uncurry throwPError) return

getWhere :: Maybe (BodySpan, SrcSpan) -> f SrcSpan -> (Lens' (f SrcSpan) SrcSpan) -> WhereSpan f
getWhere mwhre e elns = case mwhre of
        Just (bdy, spn) -> Where e (Just bdy) (srcSpanSpan (e^.elns) spn)
        Nothing         -> Where e Nothing    (e^.elns)

parseProgram :: String -> SrcLoc -> Either (Located Error) (ModuleSpan, [Located Warning])
parseProgram input loc = evalLP input loc parseModule

}
