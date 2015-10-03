module Language.Angler.ASTCompact where

import           Data.Sequence (Seq)
import           Control.Lens

data Module a
  = Module
        { _mod_name     :: String
        , _mod_body     :: Body a
        , _mod_annot    :: a
        }
  deriving Show

type Body a = Seq (BodyStmt a)

data BodyStmt a
  = Function
        { _fun_idn      :: Identifier a
        , _fun_type     :: Expression a
        , _fun_args     :: Seq (Argument a)
        , _fun_expr     :: Expression a
        , _stm_annot    :: a
        }
  | OpenType
        { _open_idn     :: Identifier a
        , _open_type    :: Expression a
        , _open_cnts    :: Seq (ExpressionBind a)
        }
  | ClosedType
        { _clsd_idn     :: Identifier a
        , _clsd_type    :: Expression a
        , _clsd_cnts    :: Seq (ExpressionBind a)
        }
  deriving Show

data Identifier a
  = Identifier
        { _idn_str      :: String
        , _idn_fix      :: Maybe Fixity
        , _idn_annot    :: a
        }
  deriving Show

data Expression a
  = Var
        { _var_str      :: String
        , _exp_annot    :: a
        }
  | Lit
        { _val_lit      :: Literal
        , _exp_annot    :: a
        }
  | Apply
        { _app_fun      :: Expression a
        , _app_over     :: Expression a
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
        { _case_args    :: Maybe (Seq (Expression a))
        , _case_cases   :: Seq (Case a)
        , _exp_annot    :: a
        }
  deriving Show

data Case a
  = Case
        { _case_pttrn   :: Seq (Argument a)
        , _case_expr    :: Expression a
        }
  deriving Show

data ExpressionBind a
  = ExpressionBind
        { _bind_name    :: String
        , _bind_expr    :: Expression a
        , _bind_annot   :: a
        }
  deriving Show

data Argument a
  = DontCare
        { _arg_annot    :: a }
  | Binding
        { _arg_bind     :: ExpressionBind a
        , _arg_annot    :: a
        }
  | ParenthesizedBinding
        { _arg_paren    :: Expression a
        , _arg_annot    :: a
        }
  deriving Show

data Literal
  = LitInt    Int
  | LitChar   Char
  | LitString String
  deriving Show

data Fixity
  = Prefix
  | Infix Associativity
  | Postfix
  | Closedfix
  deriving Show

data Associativity
  = LeftAssoc
  | RightAssoc
  | NonAssoc
  deriving Show

makeLenses ''Module
makeLenses ''BodyStmt
makeLenses ''Identifier
makeLenses ''Expression
makeLenses ''Case
makeLenses ''ExpressionBind
makeLenses ''Argument
makeLenses ''Literal
makeLenses ''Fixity
makeLenses ''Associativity
