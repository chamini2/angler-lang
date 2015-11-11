module Language.Angler.ASTCompact where

import qualified Language.Angler.Program as Program
import           Language.Angler.SrcLoc

import           Control.Lens

import           Data.Sequence (Seq)

type Identifier = Program.Identifier
idn_str :: Lens' (Identifier a) String
idn_str   = Program.idn_str
idn_annot :: Lens' (Identifier a) a
idn_annot = Program.idn_annot

data Module a
  = Module
        { _mod_name     :: String
        , _mod_body     :: Body a
        , _mod_annot    :: a
        }
  deriving Show
type ModuleSpan = Module SrcSpan

newtype Body a
  = Body { _bod_stmts   :: Seq (BodyStmt a) }
  deriving Show
type BodySpan = Body SrcSpan

data BodyStmt a
  = Function
        { _fun_idn      :: Identifier a
        , _fun_type     :: Expression a
        , _fun_expr     :: Expression a
        , _stm_annot    :: a
        }
  | Type
        { _type_idn     :: Identifier a
        , _type_type    :: Expression a
        , _type_open    :: Bool
        , _type_cnts    :: Seq (ExpressionBind a)
        , _stm_annot    :: a
        }
  | Operator
        { _oper_idn     :: Identifier a
        , _oper_fix     :: Fixity a
        , _stm_annot    :: a
        }
  deriving Show
type BodyStmtSpan = BodyStmt SrcSpan

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
        { _let_body     :: Body a
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

makeLenses ''Module
makeLenses ''BodyStmt
makeLenses ''Expression
makeLenses ''CaseAlt
makeLenses ''ExpressionBind
makeLenses ''Argument
