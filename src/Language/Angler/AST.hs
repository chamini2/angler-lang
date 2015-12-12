module Language.Angler.AST
        ( module Language.Angler.AST
        -- Program
        , Identifier(..), IdentifierSpan, idn_str, idn_annot
        , Literal(..), LiteralSpan, lit_nat, lit_char, lit_str, lit_annot
        , Fixity(..), FixitySpan, fix_assoc, fix_prec, fix_annot
        , Associativity(..)
        )
        where

import           Language.Angler.Program     ( Identifier(..), IdentifierSpan, idn_str, idn_annot
                                             , Literal(..), LiteralSpan, lit_nat, lit_char, lit_str, lit_annot
                                             , Fixity(..), FixitySpan, fix_assoc, fix_prec, fix_annot
                                             , Associativity(..) )
import           Language.Angler.SrcLoc
import           Language.Angler.ScopedTable hiding (toList)

import           PrettyShow

import           Control.Lens
import           Control.Monad               (when)

import           Data.Foldable               (toList)
import           Data.Function               (on)
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
        , _lam_expr     :: Expression a
        , _exp_annot    :: a
        }
  | Let
        { _let_tab      :: SymbolScope a
        , _let_expr     :: Expression a
        , _exp_annot    :: a
        }
  | Forall
        { _fall_typs    :: SymbolScope a
        , _fall_over    :: Expression a
        , _exp_annot    :: a
        }
  | Exists
        { _exst_typ     :: Symbol a
        , _exst_over    :: Expression a
        , _exp_annot    :: a
        }
  | Select
        { _slct_str     :: String
        , _slct_typ     :: Type a
        , _exp_annot    :: a
        }
  | Implicit
        { _impl_exprs   :: SymbolScope a
        , _exp_annot    :: a
        }
  -- | CaseOf
  --       { _case_arg     :: Expression a
  --       , _case_alts    :: Seq (CaseAlt a)
  --       , _exp_annot    :: a
  --       }
  | DontCare
        { _dnt_replace  :: Maybe String
        , _exp_annot    :: a
        }
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
dontCare = DontCare Nothing SrcSpanNoInfo

willCare :: String -> ExpressionSpan
willCare str = DontCare (Just str) SrcSpanNoInfo

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

type SymbolScope a = Scope (Symbol a)
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
  -- | SymbolOperator
  --       { _sym_idn      :: String
  --       , _sym_fix      :: Fixity a
  --       }
  | SymbolVar
        { _sym_idn      :: String
        , _sym_type     :: Type a
        , _sym_value    :: Maybe (Expression a)
        , _sym_free     :: Bool
        , _sym_parent   :: Symbol a
        }
  deriving Show
type SymbolSpan = Symbol SrcSpan

makeLenses ''Expression
makeLenses ''TypeBind
makeLenses ''Symbol

instance Eq (Symbol a) where
        (==) = (==) `on` view sym_idn

----------------------------------------
-- disjoint sets

symFind :: Symbol a -> Symbol a
symFind sym = case sym of
        SymbolFunction {} -> sym
        SymbolType {}     -> sym
        -- SymbolOperator {} -> sym
        SymbolVar {}      -> let p = sym ^?! sym_parent
                             in if p == sym then sym else symFind p

symUnion :: Symbol a -> Symbol a -> Maybe (Symbol a, Symbol a)
symUnion a b = case (symFind a, symFind b) of
        (pa@(SymbolVar {}), pb) -> Just (set sym_parent pb pa, pb)
        (pa, pb@(SymbolVar {})) -> Just (pa, set sym_parent pa pb)
        (pa, pb) | pa == pb     -> Just (pa, pb)
        _ -> Nothing

----------------------------------------

isSymFunction :: Symbol a -> Bool
isSymFunction sym = case sym of
        SymbolFunction {} -> True
        _                 -> False

isSymType :: Symbol a -> Bool
isSymType sym = case sym of
        SymbolType {} -> True
        _             -> False

-- isSymOperator :: Symbol a -> Bool
-- isSymOperator sym = case sym of
--         SymbolOperator {} -> True
--         _                 -> False

isSymVar :: Symbol a -> Bool
isSymVar sym = case sym of
        SymbolVar {} -> True
        _            -> False

symbolStr :: Symbol a -> String
symbolStr sym = case sym of
        SymbolFunction {} -> "function"
        SymbolType {}     -> (if sym^?!sym_open then "open" else "closed") ++ " type"
        -- SymbolOperator {} -> "operator"
        SymbolVar {}      -> "var"

--------------------------------------------------------------------------------
-- PrettyShow

instance PrettyShow (Expression a) where
        pshow = pshow' True
            where
                pshow' :: Bool -> Expression a -> PrettyShowed
                pshow' paren expr = pparen "(" >> exprCase >> pparen ")"
                    where
                        pparen :: String -> PrettyShowed
                        pparen s = case expr of
                                Var {}      -> return ()
                                Lit {}      -> return ()
                                Implicit {} -> return ()
                                DontCare {} -> return ()
                                TypeType {} -> return ()
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
                                        pshows line (toList bdy)
                                        lower >> line >> lower

                                        string "in "
                                        pshow x
                                Forall typs x _ -> do
                                        string "forall "
                                        pshows (string ", ") (toList typs)
                                        string " . "
                                        pshow x
                                Exists typ x _ -> do
                                        string "exists " >> pshow typ
                                        string " . " >> pshow x
                                Select str typ _ -> string "select " >> string str >> string " : " >> pshow typ
                                Implicit ims _ ->
                                        string "{" >> pshows (string ", ") (toList ims) >> string "}"
                                DontCare _ _ -> string "_"
                                Arrow f t _ -> pshow f >> string " -> " >> pshow t
                                TypeType _ -> string "Type"

instance PrettyShow (TypeBind a) where
        pshow (TypeBind name expr _) = string name >> string " : " >> pshow expr

instance PrettyShow (Symbol a) where
        pshow sym = case symFind sym of
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

                -- SymbolOperator str fix ->
                --         string "operator " >> string str >> string " " >> pshow fix

                SymbolVar str typ mval _ _ -> do
                        string str >> string " : " >> pshow typ
                        when (isJust mval) $ do
                                let Just val = mval
                                string " |= " >> pshow val
