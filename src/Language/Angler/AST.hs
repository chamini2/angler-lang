{-# LANGUAGE FlexibleContexts #-}

module Language.Angler.AST where

import           Language.Angler.SrcLoc

import           PrettyShow

import           Control.Lens
import           Control.Monad           (when)

import           Data.Foldable           (toList)
import           Data.List               (intercalate)
import           Data.Maybe              (isJust)
import           Data.Sequence           (Seq)

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

type Body a = Seq (BodyStmt a)
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

whereToLet :: ExprWhere a -> Expression a
whereToLet (Where ex mbd an) = if isJust mbd
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
  | ImplicitExpr
        { _impl_exprs   :: Implicits a
        , _exp_annot    :: a
        }
  deriving Show
type ExpressionSpan = Expression SrcSpan

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
        (<=) l r = constr l <= constr r
            where
                constr :: Fixity a -> String
                constr fix = case fix of
                        Infix a _ _ -> "Infix" ++ show a
                        Prefix  _ _ -> "Prefix"
                        Postfix _ _ -> "Postfix"
                        Closedfix _ -> "Closedfix"

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
  = LitInt
        { _lit_int      :: Int
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
makeLenses ''BodyStmt
makeLenses ''Where
makeLenses ''Expression
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

                pshows line imprts
                line >> line
                pshows (line >> line) bdy
            where
                showExports :: Traversable f => f (Identifier a) -> String
                showExports = intercalate ", " . toListOf (traverse.idn_str)

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
                                pshows line bdy
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
                                _ -> if paren then string s else return ()
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
                                        pshows line bdy
                                        lower >> line >> lower

                                        string "in "
                                        pshow x
                                Forall typs x _ -> do
                                        string "forall "
                                        pshows (string ", ") typs
                                        string " . "
                                        pshow x
                                Exists typ x _ -> do
                                        string "exists " >> pshow typ
                                        string " . " >> pshow x
                                Select typ _ -> string "select " >> pshow typ
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
                LitInt int _    -> string (show int)
                LitChar chr _   -> string (show chr)
                LitString str _ -> string (show str)
