{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Angler.Program where

import           Language.Angler.SrcLoc

import           PrettyShow

import           Control.Lens
import           Control.Monad               (when)

import           Data.Foldable               (toList)
import           Data.List                   (intercalate)
import           Data.Maybe                  (isJust)
import           Data.Sequence               (Seq)

import           Text.Megaparsec.ShowToken   (ShowToken(..))

data Identifier a
  = Identifier
        { _idn_str      :: String
        , _idn_annot    :: a
        }
  deriving Show
type IdentifierSpan = Identifier SrcSpan

data Module a
  = Module
        { _mod_name     :: String
        , _mod_exports  :: Maybe (Seq (Identifier a))   -- Nothing means to export everything
        , _mod_imports  :: Seq (Import a)
        , _mod_body     :: Body a
        , _mod_annot    :: a
        }
  deriving Show
type ModuleSpan = Module SrcSpan

data Import a
  = Import
        { _imp_path     :: FilePath
        , _imp_as       :: Maybe (Identifier a)
        , _imp_spcfc    :: Maybe (Seq (Identifier a))   -- Nothing means to import everything
        , _imp_annot    :: a
        }
  deriving Show
type ImportSpan = Import SrcSpan

newtype Body a
  = Body { _bod_stmts   :: Seq (BodyStmt a) }
  deriving Show
type BodySpan = Body SrcSpan

data BodyStmt a
  = OpenType
        { _open_idn     :: Identifier a
        , _open_type    :: ExprWhere a
        , _open_cnts    :: Maybe (Seq (TypeBind a))
        , _stm_annot    :: a
        }
  | ReopenType
        { _rpen_idn     :: Identifier a
        , _rpen_cnts    :: Seq (TypeBind a)
        , _stm_annot    :: a
        }
  | ClosedType
        { _clsd_idn     :: Identifier a
        , _clsd_type    :: ExprWhere a
        , _clsd_cnts    :: Seq (TypeBind a)
        , _stm_annot    :: a
        }
  | FunctionDecl
        { _fdec_idn     :: Identifier a
        , _fdec_type    :: ExprWhere a
        , _stm_annot    :: a
        }
  | FunctionDef
        { _fdef_args    :: Argument a
        , _fdef_expr    :: ExprWhere a
        , _stm_annot    :: a
        }
  | OperatorDef
        { _oper_idn     :: Identifier a
        , _oper_fix     :: Fixity a
        , _stm_annot    :: a
        }
  deriving Show
type BodyStmtSpan = BodyStmt SrcSpan

data Where f a
  = Where
        { _whre_insd    :: f a
        , _whre_body    :: Maybe (Body a)
        , _whre_annot   :: a
        }
  deriving Show
type WhereSpan f   = Where f SrcSpan
type ExprWhere a   = Where Expression a
type ExprWhereSpan = Where Expression SrcSpan

whereToExpression :: ExprWhere a -> Expression a
whereToExpression (Where ex mbd an) = if isJust mbd
        then let Just bd = mbd in Let bd ex an
        else ex

data Expression a
  = Var
        { _var_str      :: String
        , _exp_annot    :: a
        }
  | Lit
        { _val_lit      :: Literal a
        , _exp_annot    :: a
        }
  | Apply
        { _app_exprs    :: Seq (Expression a)
        , _exp_annot    :: a
        }
  | Lambda
        { _lam_arg      :: Argument a
        , _lam_expr     :: Expression a
        , _exp_annot    :: a
        }
  | Let
        { _let_body     :: Body a
        , _let_expr     :: Expression a
        , _exp_annot    :: a
        }
  | Forall
        { _fall_typs    :: Seq (TypeBind a)
        , _fall_expr    :: Expression a
        , _exp_annot    :: a
        }
  | Exists
        { _exst_type    :: TypeBind a
        , _exst_expr    :: Expression a
        , _exp_annot    :: a
        }
  | Select
        { _slct_type    :: TypeBind a
        , _exp_annot    :: a
        }
  | CaseOf
        { _case_arg     :: Expression a
        , _case_type    :: Expression a
        , _case_alts    :: Seq (CaseAlt a)
        , _exp_annot    :: a
        }
  | ImplicitExpr
        { _impl_exprs   :: Implicits a
        , _exp_annot    :: a
        }
  deriving Show
type ExpressionSpan = Expression SrcSpan

instance Show a => ShowToken (Expression a) where
        showToken = prettyShow

instance Show a => ShowToken [Expression a] where
        showToken = concatMap prettyShow

data CaseAlt a
  = CaseAlt
        { _csal_arg     :: Argument a
        , _csal_expr    :: ExprWhere a
        , _csal_annot   :: a
        }
  deriving Show
type CaseAltSpan = CaseAlt SrcSpan

data TypeBind a
  = TypeBind
        { _typ_idn      :: Identifier a
        , _typ_type     :: ExprWhere a
        , _typ_annot    :: a
        }
  deriving Show
type TypeBindSpan = TypeBind SrcSpan

data Associativity
  = LeftAssoc
  | RightAssoc
  | NonAssoc
  deriving (Show, Eq, Ord)

data Fixity a
  = Infix
        { _fix_assoc    :: Associativity
        , _fix_prec     :: Int
        , _fix_annot    :: a
        }
  | Prefix
        { _fix_prec     :: Int
        , _fix_annot    :: a
        }
  | Postfix
        { _fix_prec     :: Int
        , _fix_annot    :: a
        }
  | Closedfix
        { _fix_annot    :: a }
  deriving Show
type FixitySpan = Fixity SrcSpan

instance Eq (Fixity a) where
        (==) l r = l <= r && r <= l

instance Ord (Fixity a) where
        (<=) l r = case (l,r) of
                (Infix l' _ _, Infix r' _ _) -> l' <= r'
                (Infix _ _ _ , _           ) -> False

                (Prefix _ _  , Infix _ _ _ ) -> True
                (Prefix _ _  , Prefix _ _  ) -> True
                (Prefix _ _  , _           ) -> False

                (Postfix _ _ , Closedfix _ ) -> False
                (Postfix _ _ , _           ) -> True

                (Closedfix _ , _           ) -> True

data Argument a
  = VarBinding
        { _bind_idn     :: String
        , _arg_annot    :: a
        }
  | ApplyBinding
        { _paren_expr   :: Seq (Argument a)
        , _arg_annot    :: a
        }
  | DontCare
        { _arg_annot    :: a }
  deriving Show
type ArgumentSpan = Argument SrcSpan

getHeadArgumentString :: Argument a -> String
getHeadArgumentString arg = case arg of
        VarBinding str _    -> str
        ApplyBinding args _ -> getHeadArgumentString (args^?!_head)
        DontCare _          -> "_"

type Implicits a = Seq (ImplicitBinding a)
type ImplicitsSpan = Implicits SrcSpan

data ImplicitBinding a
  = ImplicitBind
        { _impl_idn     :: Identifier a
        , _impl_expr    :: Expression a
        , _impl_annot   :: a
        }
  deriving Show
type ImplicitBindingSpan = ImplicitBinding SrcSpan

data Literal a
  = LitNat
        { _lit_nat      :: Int
        , _lit_annot    :: a
        }
  | LitChar
        { _lit_char     :: Char
        , _lit_annot    :: a
        }
  | LitString
        { _lit_str      :: String
        , _lit_annot    :: a
        }
  deriving Show
type LiteralSpan = Literal SrcSpan

makeLenses ''Identifier
makeLenses ''Module
makeLenses ''Import
makeLenses ''Body
makeLenses ''BodyStmt
makeLenses ''Where
makeLenses ''Expression
makeLenses ''CaseAlt
makeLenses ''TypeBind
makeLenses ''Fixity
makeLenses ''Argument
makeLenses ''ImplicitBinding
makeLenses ''Literal

--------------------------------------------------------------------------------
-- PrettyShow

instance PrettyShow (Module a) where
        pshow (Module _ mexprts imprts bdy _) = do
                when (isJust mexprts) $ do
                        let Just exprts = mexprts
                        string "export (" >> string (showExports exprts) >> string ")"
                        line >> line

                when (length imprts > 0) $ do
                        pshows line imprts
                        line >> line
                pshow bdy
            where
                showExports :: Traversable f => f (Identifier a) -> String
                showExports = intercalate ", " . toListOf (traverse.idn_str)

instance PrettyShow (Body a) where
        pshow (Body stmts) = pshows line stmts

instance PrettyShow (Import a) where
        pshow (Import path mas mspec _) = do
                string "import " >> string path
                when (isJust mas) $ do
                        let Just as = mas
                        string " as " >> lstring idn_str as
                when (isJust mspec) $ do
                        let Just spec = mspec
                        string " (" >> string (showSpecs spec) >> string ")"
            where
                showSpecs :: Traversable f => f (Identifier a) -> String
                showSpecs = intercalate ", " . toListOf (traverse.idn_str)

instance PrettyShow (BodyStmt a) where
        pshow bdst = case bdst of
                OpenType idn typ mcns _ -> do
                        string "open " >> lstring idn_str idn
                        string " : " >> pshow typ
                        when (isJust mcns) $ do
                                let Just cns = mcns
                                string " with"
                                raise >> line >> pshows line cns >> lower
                ReopenType idn cns _ -> do
                        string "reopen " >> lstring idn_str idn >> string " with"
                        raise >> line >> pshows line cns >> lower
                ClosedType idn typ cns _ -> do
                        string "closed " >> lstring idn_str idn
                        string " : " >> pshow typ >> string " with"
                        raise >> line >> pshows line cns >> lower
                FunctionDecl idn typ _ -> do
                        lstring idn_str idn
                        string " : " >> pshow typ
                FunctionDef args expr _ -> do
                        pshow args
                        string " = " >> pshow expr
                OperatorDef idn fx _ -> do
                        string "operator " >> lstring idn_str idn >> string " "
                        pshow fx

instance PrettyShow (f a) => PrettyShow (Where f a) where
        pshow whre = case whre of
                Where insd mbdy _ -> do
                        pshow insd
                        when (isJust mbdy) $ do
                                let Just bdy = mbdy
                                raise >> line
                                string "where"

                                raise >> line
                                pshow bdy
                                lower >> lower

instance PrettyShow (Expression a) where
        pshow = pshow' False
            where
                pshow' :: Bool -> Expression a -> PrettyShowed
                pshow' paren expr = pparen "(" >> exprCase >> pparen ")"
                    where
                        pparen :: String -> PrettyShowed
                        pparen s = case expr of
                                Var {}          -> return ()
                                Lit {}          -> return ()
                                ImplicitExpr {} -> return ()
                                _ -> when paren (string s)
                        exprCase :: PrettyShowed
                        exprCase = case expr of
                                Var str _  -> string "«" >> string str >> string "»"
                                Lit lit _  -> pshow lit
                                Apply xs _ -> pshows' " " xs
                                Lambda arg x _ -> do
                                        string "\\ " >> pshow arg
                                        string " -> " >> pshow x
                                Let bdy x _ -> do
                                        raise >> line
                                        string "let"

                                        raise >> line
                                        pshow bdy
                                        lower >> line >> lower

                                        string "in " >> pshow x
                                Forall typs x _ -> do
                                        string "forall "
                                        pshows (string ", ") typs
                                        string " . "
                                        pshow x
                                Exists typ x _ -> do
                                        string "exists " >> pshow typ
                                        string " . " >> pshow x
                                Select typ _ -> string "select " >> pshow typ
                                CaseOf arg typ alts _ -> do
                                        raise >> line
                                        string "case " >> pshow arg
                                        string " : " >> pshow typ >> string " of"

                                        raise >> line
                                        pshows line alts
                                        lower >> lower >> line
                                ImplicitExpr ims _ ->
                                        string "{" >> pshows (string ", ") ims >> string "}"
                        pshows' :: Foldable f => String -> f (Expression a) -> PrettyShowed
                        pshows' str exprs = case toList exprs of
                                p : ps -> pshow' True p >> mapM_ go ps
                                _      -> return ()
                            where
                                go :: Expression a -> PrettyShowed
                                go x = string str >> pshow' True x

instance PrettyShow Associativity where
        pshow assoc = case assoc of
                LeftAssoc  -> string "L"
                RightAssoc -> string "R"
                NonAssoc   -> string "N"

instance PrettyShow (Fixity a) where
        pshow fix = case fix of
                Infix assoc pr _ -> string "infix" >> pshow assoc >> string (" " ++ show pr)
                Prefix      pr _ -> string "prefix" >> string (" " ++ show pr)
                Postfix     pr _ -> string "postfix" >> string (" " ++ show pr)
                Closedfix      _ -> string "closed"

instance PrettyShow (CaseAlt a) where
        pshow (CaseAlt arg expr _) = pshow arg >> string " = " >> pshow expr

instance PrettyShow (TypeBind a) where
        pshow (TypeBind idn typ _) = lstring idn_str idn >> string " : " >> pshow typ

instance PrettyShow (Argument a) where
        pshow = pshow' False
            where
                pshow' :: Bool -> Argument a -> PrettyShowed
                pshow' paren arg = pparen "(" >> argCase >> pparen ")"
                    where
                        pparen :: String -> PrettyShowed
                        pparen s = case arg of
                                ApplyBinding {} -> if paren then string s else return ()
                                _ -> return ()
                        argCase :: PrettyShowed
                        argCase = case arg of
                                VarBinding idn _    -> string idn
                                DontCare _          -> string "_"
                                ApplyBinding args _ -> pshows' " " args
                        pshows' :: Foldable f => String -> f (Argument a) -> PrettyShowed
                        pshows' str args = case toList args of
                                p : ps -> pshow' True p >> mapM_ go ps
                                _      -> return ()
                            where
                                go :: Argument a -> PrettyShowed
                                go x = string str >> pshow' True x

instance PrettyShow (ImplicitBinding a) where
        pshow (ImplicitBind idn expr _) = lstring idn_str idn >> string " = " >> pshow expr

instance PrettyShow (Literal a) where
        pshow lit = case lit of
                LitNat int _    -> string (show int)
                LitChar chr _   -> string (show chr)
                LitString str _ -> string (show str)
