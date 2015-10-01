{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Angler.AST where

import           Language.Angler.SrcLoc

import           Control.Lens
import           Control.Monad           (when)
import           Control.Monad.State     (State, execState)

import           Data.Default            (Default(..))
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
        { _impr_path    :: FilePath
        , _impr_as      :: Maybe (Identifier a)
        , _impr_spcfc   :: Maybe (Seq (Identifier a))   -- Nothing means to import everything
        , _impr_annot   :: a
        }
  deriving Show
type ImportSpan = Import SrcSpan

type Body a = Seq (BodyStmt a)
type BodySpan = Body SrcSpan

data BodyStmt a
  = OpenType
        { _open_id      :: Identifier a
        , _open_type    :: ExprWhere a
        , _open_cnstrc  :: Maybe (Seq (TypeBind a))
        , _stm_annot    :: a
        }
  | ReopenType
        { _rpen_id      :: Identifier a
        , _rpen_cnstrc  :: Seq (TypeBind a)
        , _stm_annot    :: a
        }
  | ClosedType
        { _clsd_id      :: Identifier a
        , _clsd_type    :: ExprWhere a
        , _clsd_cnstrc  :: Seq (TypeBind a)
        , _stm_annot    :: a
        }
  | FunctionDecl
        { _fdec_id      :: Identifier a
        , _fdec_type    :: ExprWhere a
        , _stm_annot    :: a
        }
  | FunctionDef
        { _fdef_args    :: Seq (Argument a)
        , _fdef_expr    :: ExprWhere a
        , _stm_annot    :: a
        }
  | OperatorDef
        { _fixd_id      :: Identifier a
        , _fixd_fix     :: Fixity a
        , _fixd_prec    :: Maybe Int
        , _fixd_annot   :: a
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
        { _typ_id       :: Identifier a
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
  = Prefix
        { _fix_annot    :: a }
  | Infix
        { _fix_assoc    :: Associativity
        , _fix_annot    :: a
        }
  | Postfix
        { _fix_annot    :: a }
  | Closedfix
        { _fix_annot    :: a }
  deriving Show
type FixitySpan = Fixity SrcSpan

instance Eq (Fixity a) where
        (==) a b = a <= b && b <= a

instance Ord (Fixity a) where
        (<=) a b = constr a <= constr b
            where
                constr :: Fixity a -> String
                constr fix = case fix of
                        Prefix    _ -> "Prefix"
                        Postfix   _ -> "Postfix"
                        Closedfix _ -> "Closedfix"
                        Infix ass _ -> "Infix" ++ show ass

data Argument a
  = Binding
        { _bind_id      :: Identifier a
        , _arg_annot    :: a
        }
  | DontCare
        { _arg_annot    :: a }
  | ParenthesizedBinding
        { _paren_expr   :: Seq (Argument a)
        , _arg_annot    :: a
        }
  deriving Show
type ArgumentSpan = Argument SrcSpan

type Implicits a = Seq (ImplicitBinding a)
type ImplicitsSpan = Implicits SrcSpan

data ImplicitBinding a
  = ImplicitBind
        { _impl_id      :: Identifier a
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

type Indentation = Int
type PrettyShowMonad = State PrettyShowState ()

data PrettyShowState
  = PrettyShowState
        { _ps_indent    :: Indentation
        , _ps_lines     :: [(Indentation, String)]
        }

instance Default PrettyShowState where
        def = PrettyShowState
                { _ps_indent = 4
                , _ps_lines  = [(4, "")]
                }

makeLenses ''PrettyShowState

class PrettyShow a where
        pshow :: a -> PrettyShowMonad

prettyShow :: PrettyShow a => a -> String
prettyShow = prettyShowIndent 0 "    "

prettyShowIndent :: PrettyShow a => Indentation -> String -> a -> String
prettyShowIndent n str = showLines . _ps_lines . flip execState initialST . pshow
    where
        showLines :: [(Indentation, String)] -> String
        showLines = concatMap (\(ind, s) -> tabs ind ++ s ++ "\n") . reverse
        tabs :: Indentation -> String
        tabs ind = concat (replicate ind str)
        initialST :: PrettyShowState
        initialST = def
                { _ps_indent = n
                , _ps_lines  = [(n, "")]
                }

----------------------------------------

pshows :: (PrettyShow a, Foldable f) => PrettyShowMonad -> f a -> PrettyShowMonad
pshows act xs = case toList xs of
        p : ps -> pshow p >> mapM_ (\x -> act >> pshow x) ps
        _      -> return ()

string :: String -> PrettyShowMonad
string str = ps_lines._head._2 %= (++ str)

lstring :: Getting String s String -> s -> PrettyShowMonad
lstring lns = string . view lns

line :: PrettyShowMonad
line = use ps_indent >>= \n -> ps_lines %= cons (n, "")

raise :: PrettyShowMonad
raise = ps_indent += 1

lower :: PrettyShowMonad
lower = ps_indent -= 1

----------------------------------------

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
                        pshows (string " ") args
                        string " = " >> pshow expr
                OperatorDef idn fx mint _ -> do
                        string "fixity " >> lstring idn_str idn >> string " "
                        pshow fx
                        when (isJust mint) $ do
                                let Just int = mint
                                string " " >> string (show int)

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
        pshow expr = case expr of
                Var str _           -> string "«" >> string str >> string "»"
                Lit lit _           -> pshow lit
                Apply exprs   _     -> pshows (string " ") exprs
                Lambda arg expr' _  -> string "\\ " >> pshow arg >> string " -> " >> pshow expr'
                Let bdy expr' _     -> do
                        raise >> line
                        string "let"

                        raise >> line
                        pshows line bdy
                        lower >> line

                        string "in "
                        pshow expr'
                        lower
                Forall typs expr' _ -> string "forall " >> pshows' ", " typs >> string " . "
                                                        >> pshow expr'
                Exists typ expr' _  -> string "exists " >> pshow typ >> string ";"
                                                        >> pshow expr'
                Select typ _        -> string "select " >> pshow typ
                ImplicitExpr imps _ -> string "{" >> pshows' " " imps >> string "}"
            where
                pshows' :: (PrettyShow a, Foldable f) => String -> f a -> PrettyShowMonad
                pshows' = pshows . string

instance PrettyShow Associativity where
        pshow assoc = case assoc of
                LeftAssoc  -> string "L"
                RightAssoc -> string "R"
                NonAssoc   -> string "N"

instance PrettyShow (Fixity a) where
        pshow fix = case fix of
                Prefix _      -> string "prefix"
                Infix assoc _ -> string "infix" >> pshow assoc
                Postfix _     -> string "postfix"
                Closedfix _   -> string "closed"

instance PrettyShow (TypeBind a) where
        pshow (TypeBind idn typ _) = lstring idn_str idn >> string " : " >> pshow typ

instance PrettyShow (Argument a) where
        pshow arg = case arg of
                Binding idn _               -> lstring idn_str idn
                DontCare _                  -> string "_"
                ParenthesizedBinding args _ -> string "(" >> pshows (string " ") args >> string ")"

instance PrettyShow (ImplicitBinding a) where
        pshow (ImplicitBind idn expr _) = lstring idn_str idn >> string " = " >> pshow expr

instance PrettyShow (Literal a) where
        pshow lit = case lit of
                LitInt int _    -> string (show int)
                LitChar chr _   -> string (show chr)
                LitString str _ -> string (show str)
