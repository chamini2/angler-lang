module Language.Angler.AST where

import qualified Language.Angler.Program as Program
import           Language.Angler.SrcLoc
import           Language.Angler.ScopedTable

import           Control.Lens

import           Data.Sequence (Seq)

--------------------------------------------------------------------------------
-- abstract syntax tree

type Identifier = Program.Identifier
idn_str :: Lens' (Identifier a) String
idn_str = Program.idn_str
idn_annot :: Lens' (Identifier a) a
idn_annot = Program.idn_annot

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
        { _app_fun      :: Expression a
        , _app_over     :: Expression a
        , _exp_annot    :: a
        }
  | Lambda
        { _lam_arg      :: String
        , _lam_arg_type :: Expression a
        , _lam_expr     :: Expression a
        , _exp_annot    :: a
        }
  | Let
        { _let_tab      :: SymbolTable a
        , _let_expr     :: Expression a
        , _exp_annot    :: a
        }
  | Forall
        { _fall_typs    :: Seq (ExpressionBind a)
        , _fall_over    :: Expression a
        , _exp_annot    :: a
        }
  | Exists
        { _exst_type    :: ExpressionBind a
        , _exst_over    :: Expression a
        , _exp_annot    :: a
        }
  | Select
        { _slct_type    :: ExpressionBind a
        , _exp_annot    :: a
        }
  | Implicit
        { _impl_exprs   :: Seq (ExpressionBind a)
        , _exp_annot    :: a
        }
  | CaseOf
        { _case_arg     :: Expression a
        , _case_alts    :: Seq (CaseAlt a)
        , _exp_annot    :: a
        }
  deriving Show
type ExpressionSpan = Expression SrcSpan

data CaseAlt a
  = CaseAlt
        { _calt_arg     :: Argument a
        , _calt_expr    :: Expression a
        , _calt_annot   :: a
        }
  deriving Show
type CaseAltSpan = CaseAlt SrcSpan

data ExpressionBind a
  = ExpressionBind
        { _bind_name    :: String
        , _bind_expr    :: Expression a
        , _bind_annot   :: a
        }
  deriving Show
type ExpressionBindSpan = ExpressionBind SrcSpan

type Associativity = Program.Associativity

type Fixity = Program.Fixity
type FixitySpan = Fixity SrcSpan
fix_assoc :: Traversal' (Fixity a) Associativity
fix_assoc = Program.fix_assoc
fix_prec :: Traversal' (Fixity a) Int
fix_prec = Program.fix_prec
fix_annot :: Lens' (Fixity a) a
fix_annot = Program.fix_annot

data Argument a
  = NameBinding
        { _arg_bind     :: String
        , _arg_annot    :: a
        }
  | Pattern
        { _arg_pattern  :: Expression a
        , _arg_annot    :: a
        }
  | DontCare
        { _arg_annot    :: a }
  deriving Show
type ArgumentSpan = Argument SrcSpan

type Literal = Program.Literal
type LiteralSpan = Literal SrcSpan

--------------------------------------------------------------------------------
-- symbol table

type SymbolTable a = ScopedTable (Symbol a)
type SymbolTableSpan = SymbolTable SrcSpan

data Symbol a
  = SymbolFunction
        { _sym_idn      :: Identifier a
        , _sym_type     :: Expression a
        , _sym_def      :: Maybe (Expression a)
        }
  | SymbolType
        { _sym_idn      :: Identifier a
        , _sym_type     :: Expression a
        , _sym_cons     :: Seq (ExpressionBind a)
        , _sym_open     :: Bool
        }
  | SymbolArgument
        { _sym_idn      :: Identifier a
        , _sym_type     :: Expression a
        -- , _sym_val      :: Expression a
        }
  deriving Show
type SymbolSpan = Symbol SrcSpan

makeLenses ''Expression
makeLenses ''CaseAlt
makeLenses ''ExpressionBind
makeLenses ''Argument
makeLenses ''Symbol

-- isSymbolFunction :: Symbol a -> Bool
-- isSymbolFunction sym = case sym of
--         SymbolFunction {} -> True
--         _                 -> False
--
-- isSymbolType :: Symbol a -> Bool
-- isSymbolType sym = case sym of
--         SymbolType {} -> True
--         _             -> False
--
-- isSymbolArgument :: Symbol a -> Bool
-- isSymbolArgument sym = case sym of
--         SymbolArgument {} -> True
--         _                 -> False
--
-- insertFunctionDecl :: Identifier a -> Expression a -> SymbolTable a -> Either Error (SymbolTable a)
-- insertFunctionDecl idn typ = safeInsert (view idn_str idn) (SymbolFunction idn typ Nothing)

-- insertFunctionDef :: Identifier a -> [Argument a] -> Expression a -> SymbolTable a -> Either Error (SymbolTable a)
-- insertFunctionDef idn args expr tab = do
--         let str = view idn_str idn
--
--         unless (elemInCurrentScope str tab) $ Left (error "insertFunctionDef")
--         let Just tabSym = lookup str tab
--
--         unless (isSymbolFunction tabSym) $ Left (error "insertFunctionDef 2")
--         let SymbolFunction _ typ mexpr = tabSym
--         let expr' = maybe expr (addCaseAlt args expr) mexpr
--
--     where
--         addCaseAlt :: [Argument a] -> Expression a -> Expression a -> Expression a
--         addCaseAlt as x old = case old of
--                 Lambda larg ltyp lexpr ann -> case lexpr of
--                         CaseOf arg calts ann -> CaseOf arg (buildAlt as x <| calts) ann
--                         _ -> CaseOf x
