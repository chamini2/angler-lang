module Language.Angler.ASTCompact where

import qualified Language.Angler.AST as AST

import           Data.Sequence (Seq)
import           Control.Lens

type Identifier = AST.Identifier
idn_str :: Lens' (Identifier a) String
idn_str   = AST.idn_str
idn_annot :: Lens' (Identifier a) a
idn_annot = AST.idn_annot

data Module a
  = Module
        { _mod_name     :: String
        , _mod_body     :: Body a
        , _mod_annot    :: a
        }
  deriving Show

newtype Body a
  = Body { _bod_stmts   :: Seq (BodyStmt a) }
  deriving Show

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
        { _case_args    :: Seq (Expression a)
        , _case_cases   :: Seq (Case a)
        , _exp_annot    :: a
        }
  deriving Show

data Case a
  = Case
        { _case_pttrn   :: Seq (Argument a)
        , _case_expr    :: Expression a
        , _case_annot   :: a
        }
  deriving Show

data ExpressionBind a
  = ExpressionBind
        { _bind_name    :: String
        , _bind_expr    :: Expression a
        , _bind_annot   :: a
        }
  deriving Show

type Associativity = AST.Associativity

type Fixity = AST.Fixity
fix_assoc :: Traversal' (Fixity a) Associativity
fix_assoc = AST.fix_assoc
fix_prec :: Traversal' (Fixity a) Int
fix_prec = AST.fix_prec
fix_annot :: Lens' (Fixity a) a
fix_annot = AST.fix_annot

data Argument a
  = VarBinding
        { _arg_bind     :: ExpressionBind a
        , _arg_annot    :: a
        }
  | ApplyBinding
        { _arg_paren    :: Expression a
        , _arg_annot    :: a
        }
  | DontCare
        { _arg_annot    :: a }
  deriving Show

type Literal = AST.Literal

makeLenses ''Module
makeLenses ''BodyStmt
makeLenses ''Expression
makeLenses ''Case
makeLenses ''ExpressionBind
makeLenses ''Argument
