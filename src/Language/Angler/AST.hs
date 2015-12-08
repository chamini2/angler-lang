module Language.Angler.AST
        ( module Language.Angler.AST
        -- Program
        , Identifier(..), IdentifierSpan, idn_str, idn_annot
        , Literal(..), LiteralSpan, lit_int, lit_char, lit_str, lit_annot
        , Fixity(..), FixitySpan, fix_assoc, fix_prec, fix_annot
        , Associativity(..)
        )
        where

import           Language.Angler.Program     ( Identifier(..), IdentifierSpan, idn_str, idn_annot
                                             , Literal(..), LiteralSpan, lit_int, lit_char, lit_str, lit_annot
                                             , Fixity(..), FixitySpan, fix_assoc, fix_prec, fix_annot
                                             , Associativity(..) )
import           Language.Angler.SrcLoc
import           Language.Angler.ScopedTable

import           PrettyShow

import           Control.Lens
import           Control.Monad               (when)

import           Data.Maybe                  (isJust)
import           Data.Sequence               (Seq)

--------------------------------------------------------------------------------
-- abstract syntax tree

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
        { _lam_arg      :: Argument a
        -- , _lam_arg_type :: Type a
        , _lam_expr     :: Expression a
        , _exp_annot    :: a
        }
  | Let
        { _let_tab      :: SymbolTable a
        , _let_expr     :: Expression a
        , _exp_annot    :: a
        }
  | Forall
        { _fall_typs    :: SymbolTable a
        , _fall_over    :: Expression a
        , _exp_annot    :: a
        }
  | Exists
        { _exst_typs    :: SymbolTable a
        , _exst_over    :: Expression a
        , _exp_annot    :: a
        }
  | Select
        { _slct_type    :: Symbol a
        , _exp_annot    :: a
        }
  | Implicit
        { _impl_exprs   :: SymbolTable a
        , _exp_annot    :: a
        }
  -- | CaseOf
  --       { _case_arg     :: Expression a
  --       , _case_alts    :: Seq (CaseAlt a)
  --       , _exp_annot    :: a
  --       }
  | DontCare
        { _exp_annot    :: a }
  -- hard-coded parts of the language
  | Arrow
        { _arr_from     :: Expression a
        , _arr_to       :: Expression a
        , _exp_annot    :: a
        }
  | TypeType
        { _exp_annot    :: a }
  deriving Show
type ExpressionSpan = Expression SrcSpan

typeType :: ExpressionSpan
typeType = TypeType SrcSpanNoInfo

dontCare :: ExpressionSpan
dontCare = DontCare SrcSpanNoInfo

type Type           = Expression
type TypeSpan       = Expression SrcSpan

type Argument       = Expression
type ArgumentSpan   = Expression SrcSpan

-- data CaseAlt a
--   = CaseAlt
--         { _calt_arg     :: Argument a
--         , _calt_expr    :: Expression a
--         , _calt_annot   :: a
--         }
--   deriving Show
-- type CaseAltSpan = CaseAlt SrcSpan

data TypeBind a
  = TypeBind
        { _bind_name    :: String
        , _bind_type    :: Expression a
        , _bind_annot   :: a
        }
  deriving Show
type TypeBindSpan = TypeBind SrcSpan

--------------------------------------------------------------------------------
-- symbol table

type SymbolTable a = ScopedTable (Symbol a)
type SymbolTableSpan = SymbolTable SrcSpan

data Symbol a
  = SymbolFunction
        { _sym_idn      :: String
        , _sym_type     :: Type a
        , _sym_defs     :: Seq (Seq (Argument a), Expression a)
        }
  | SymbolType
        { _sym_idn      :: String
        , _sym_data     :: String
        , _sym_type     :: Type a
        , _sym_open     :: Bool
        }
  | SymbolVar
        { _sym_idn      :: String
        , _sym_type     :: Type a
        , _sym_may_val  :: Maybe (Expression a)
        , _sym_free     :: Bool
        }
  | SymbolOperator
        { _sym_idn      :: String
        , _sym_fix      :: Fixity a
        }
  deriving Show
type SymbolSpan = Symbol SrcSpan

makeLenses ''Expression
makeLenses ''TypeBind
makeLenses ''Symbol

isSymFunction :: Symbol a -> Bool
isSymFunction sym = case sym of
        SymbolFunction {} -> True
        _                 -> False

isSymType :: Symbol a -> Bool
isSymType sym = case sym of
        SymbolType {} -> True
        _             -> False

isSymVar :: Symbol a -> Bool
isSymVar sym = case sym of
        SymbolVar {} -> True
        _            -> False

isSymOperator :: Symbol a -> Bool
isSymOperator sym = case sym of
        SymbolOperator {} -> True
        _                 -> False

symbolStr :: Symbol a -> String
symbolStr sym = case sym of
        SymbolFunction {}    -> "function"
        SymbolType {}        -> (if sym^?!sym_open then "open" else "closed") ++ " type"
        SymbolVar {}         -> "var"
        SymbolOperator {}    -> "operator"

--------------------------------------------------------------------------------
-- PrettyShow

instance PrettyShow (Expression a) where
        pshow = pshow' False
            where
                pshow' :: Bool -> Expression a -> PrettyShowed
                pshow' paren expr = pparen "(" >> exprCase >> pparen ")"
                    where
                        pparen :: String -> PrettyShowed
                        pparen s = case expr of
                                Var {}      -> return ()
                                Lit {}      -> return ()
                                Implicit {} -> return ()
                                _ -> when paren (string s)
                        exprCase :: PrettyShowed
                        exprCase = case expr of
                                Var str _  -> string "«" >> string str >> string "»"
                                Lit lit _  -> pshow lit
                                Apply fn ov _ -> pshow' True fn >> string " " >> pshow' True ov
                                Lambda arg x _ -> do
                                        string "\\ " >> pshow arg
                                        string " -> " >> pshow x
                                Let bdy x _ -> do
                                        raise >> line
                                        string "let"

                                        raise >> line
                                        pshow bdy
                                        lower >> line >> lower

                                        string "in "
                                        pshow x
                                Forall typs x _ -> do
                                        string "forall "
                                        pshows (string ", ") (snd <$> toList typs)
                                        string " . "
                                        pshow x
                                Exists typ x _ -> do
                                        string "exists " >> pshow typ
                                        string " . " >> pshow x
                                Select typ _ -> string "select " >> pshow typ
                                Implicit ims _ ->
                                        string "{" >> pshows (string ", ") (snd <$> toList ims) >> string "}"
                                DontCare _ -> string "_"
                                Arrow f t _ -> pshow f >> string " -> " >> pshow t
                                TypeType _ -> string "Type"

instance PrettyShow (TypeBind a) where
        pshow (TypeBind name expr _) = string name >> string " : " >> pshow expr

instance PrettyShow (Symbol a) where
        pshow sym = case sym of
                SymbolFunction str typ defs -> do
                        string str >> string " : " >> pshow typ
                        raise >> pshows' defs >> lower
                    where
                        pshows' :: (PrettyShow a, Foldable f) => f (f a, a) -> PrettyShowed
                        pshows' = mapM_ $ \(args, expr) -> do
                                line
                                string str >> string " "
                                pshows (string " , ") args
                                string " = "
                                pshow expr

                SymbolType str _ typ open -> do
                        string (if open then "open" else "closed")
                        string " "
                        string str >> string " : " >> pshow typ

                SymbolVar str typ mval _ -> do
                        string str >> string " : " >> pshow typ
                        when (isJust mval) $ do
                                let Just val = mval
                                line
                                string str >> string " = " >> pshow val

                SymbolOperator str fix ->
                        string "operator " >> string str >> string " " >> pshow fix
