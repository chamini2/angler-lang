module Language.Angler.AST2 where

data Function
  = Function
        { _fun_name     :: Maybe String
        , _fun_type     :: Expression
        , _fun_args     :: Seq Argument
        , _fun_expr     :: Expression
        }
  deriving Show

data DataType
  = DataType
        { _dat_name     :: String
        , _dat_type     :: Expression
        , _dat_conss    :: Seq Type
        }
  deriving Show

data Expression
  = Var
        { _var_str      :: String }
  | Lit
        { _val_lit      :: Literal a }
  | Apply
        { _app_fun      :: Expression
        , _app_over     :: Expression
        }
  | Lambda
        { _lam_arg      :: Argument
        , _lam_expr     :: Expression
        }
  | Forall
        { _fall_typs    :: Seq Type
        , _fall_over    :: Expression
        }
  | Exists
        { _exst_type    :: Type
        , _exst_over    :: Expression
        }
  | Select
        { _slct_type    :: (String, Expression) }
  | Implicit
        { _impl_exprs   :: Seq (String, Expression) }

data Type
  = Type
        { _type_name    :: String
        , _type_expr    :: Expression
        }

data Argument
  = DontCare
  | FreeVar
        { _free_name    :: String }
  | PatternMatch
        { _ptrn_expr    :: Expression }
