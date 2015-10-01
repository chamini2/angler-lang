module Language.Angler.ASTCompact where

data BodyStmt
  = Function
        { _fun_name     :: String
        , _fun_type     :: Expression
        , _fun_args     :: Seq Argument
        , _fun_expr     :: Expression
        , _fun_fix      :: Maybe Fixity
        }
  -- | TypeDecl
  --       { _decl_name    :: String
  --       , _decl_type    :: Expression
  --       }
  | OpenType
        { _open_name    :: String
        , _open_type    :: Expression
        , _open_cnts    :: Seq ExpressionBind
        }
  | ClosedType
        { _clsd_name    :: String
        , _clsd_type    :: Expression
        , _clsd_cnts    :: Seq ExpressionBind
        }
  deriving Show

data Expression
  = Var
        { _var_str      :: String }
  | Lit
        { _val_lit      :: Literal }
  | Apply
        { _app_fun      :: Expression
        , _app_over     :: Expression
        }
  | Lambda
        { _lam_arg      :: Argument
        , _lam_expr     :: Expression
        }
  | Let
        { _let_body     :: Body
        , _let_expr     :: Expression
        }
  | Forall
        { _fall_typs    :: Seq ExpressionBind
        , _fall_over    :: Expression
        }
  | Exists
        { _exst_type    :: ExpressionBind
        , _exst_over    :: Expression
        }
  | Select
        { _slct_type    :: ExpressionBind }
  | Implicit
        { _impl_exprs   :: Seq ExpressionBind }

data ExpressionBind
  = ExpressionBind
        { _bind_name    :: String
        , _bind_expr    :: Expression
        }

data Argument
  = DontCare
  | Binding
        { _binding      :: ExpressionBind }
  | ParenthesizedBinding
        { _paren_expr   :: Expression }
